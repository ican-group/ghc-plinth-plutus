{-| The on-chain cost of a Plinth script: size, execution units and fees.

The evaluation uses the functions of the ledger (deserialiseScript,
mkEvaluationContext, evaluateScriptCounting) with pinned parameters, see
Note [Pinned cost models] in "Plinth.Cost.Params". -}
module Plinth.Cost
  ( -- * Cost
    Cost (..)
  , CostError (..)
  , evalCost
  , evalSerialisedCost

    -- * Size
  , scriptSize
  , serialisedSize

    -- * Fees and limits
  , executionFee
  , txSizeFee
  , refScriptFee
  , TxFractions (..)
  , txFractions

    -- * Rendering
  , prettyCost
  , prettyExecution
  , prettySize
  , prettyBytes
  , prettyCostError

    -- * Parameters
  , module Plinth.Cost.Params
  ) where

import Control.Monad.Except (runExcept)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Bifunctor (first)
import Data.ByteString.Short qualified as SBS
import Data.Int (Int64)
import Data.SatInt (unSatInt)
import Data.Text (Text)
import Data.Text qualified as Text
import Plinth.Cost.Params
import PlutusLedgerApi.Common
  ( CostModelApplyError
  , Data
  , EvaluationError (InvalidReturnValue)
  , ExBudget (..)
  , ExCPU (..)
  , ExMemory (..)
  , PlutusLedgerLanguage (PlutusV3)
  , ScriptDecodeError
  , SerialisedScript
  , VerboseMode (Verbose)
  , deserialiseScript
  , evaluateScriptCounting
  , serialiseCompiledCode
  )
-- Not PlutusLedgerApi.V3: see Note [Narrow ledger imports]
import PlutusLedgerApi.V3.EvaluationContext qualified as V3
import PlutusTx.Code (CompiledCode)

{- Note [Narrow ledger imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC loads the family instance modules of the whole import closure of each
imported module. PlutusLedgerApi.V3 brings the ScriptContext types of V1 to
V3, with many derived instances, and plinthc exposes all unfoldings: their
interfaces are large. Thus this module imports only the small ledger modules
that it needs. See also Note [Cost reporter hook] in
plutus-tx-plugin:PlutusTx.Plugin.Common.
-}

-- | The cost of one evaluation of a script.
data Cost = Cost
  { costParams :: CostParams
  -- ^ the parameters of the evaluation
  , costSizeBytes :: Int
  -- ^ the size of the serialised script
  , costBudget :: ExBudget
  -- ^ the execution units that the evaluation used
  , costLogs :: [Text]
  -- ^ the trace messages
  }
  deriving stock (Show)

data CostError
  = -- | The parameters do not give an evaluation context.
    CostModelError CostModelApplyError
  | -- | The ledger rejects the script at this PV (e.g. a builtin that is not available).
    DecodeError ScriptDecodeError
  | -- | The script fails. The trace messages are given.
    EvalError EvaluationError [Text]
  deriving stock (Show)

-- | The size in bytes of a compiled script, as the ledger counts it.
scriptSize :: CompiledCode a -> Int
scriptSize = serialisedSize . serialiseCompiledCode

-- | The size in bytes of a serialised script.
serialisedSize :: SerialisedScript -> Int
serialisedSize = SBS.length

{-| Evaluate a PlutusV3 script applied to the arguments, as the ledger
does. A V3 validator takes one argument: the script context. -}
evalCost :: CostParams -> CompiledCode a -> [Data] -> Either CostError Cost
evalCost params = evalSerialisedCost params . serialiseCompiledCode

-- | 'evalCost' for a serialised script.
evalSerialisedCost :: CostParams -> SerialisedScript -> [Data] -> Either CostError Cost
evalSerialisedCost params ser args = do
  -- The warnings tell that mainnet sets fewer parameters than the ledger
  -- expects. See Note [Pinned cost models] in Plinth.Cost.Params.
  (ctx, _warnings) <-
    first CostModelError . runExcept . runWriterT $
      V3.mkEvaluationContext (cpCostModelV3 params)
  script <- first DecodeError (deserialiseScript PlutusV3 pv ser)
  let (logs, res) = evaluateScriptCounting PlutusV3 pv Verbose ctx script args
  case res of
    Left err -> Left (EvalError err logs)
    Right budget ->
      Right
        Cost
          { costParams = params
          , costSizeBytes = serialisedSize ser
          , costBudget = budget
          , costLogs = logs
          }
  where
    pv = cpProtocolVersion params

{-| The execution fee in lovelace (not rounded). The ledger rounds up the
sum for all the scripts of a transaction. -}
executionFee :: CostParams -> ExBudget -> Rational
executionFee params (ExBudget cpu mem) =
  cpPriceSteps params * fromIntegral (cpu_int cpu)
    + cpPriceMem params * fromIntegral (mem_int mem)

-- | The fee in lovelace for the bytes of a script in the transaction.
txSizeFee :: CostParams -> Int -> Integer
txSizeFee params n = cpTxFeePerByte params * fromIntegral n

{-| The fee in lovelace for a reference script of this size. The ledger
applies it to the total size of the reference scripts of a transaction: the
price increases by 1.2 for each 25 KiB. -}
refScriptFee :: CostParams -> Int -> Integer
refScriptFee params = floor . go 0 (cpRefScriptCostPerByte params) . fromIntegral
  where
    tier = 25600 :: Integer
    go :: Rational -> Rational -> Integer -> Rational
    go acc price n
      | n <= tier = acc + fromIntegral n * price
      | otherwise = go (acc + fromIntegral tier * price) (price * 1.2) (n - tier)

-- | Parts of the limits of one transaction.
data TxFractions = TxFractions
  { txfCpu :: Double
  , txfMem :: Double
  , txfSize :: Double
  }
  deriving stock (Show)

-- | The parts of the transaction limits that a cost uses.
txFractions :: Cost -> TxFractions
txFractions c =
  TxFractions
    { txfCpu = ratio (cpu_int cpu) (cpu_int max_cpu)
    , txfMem = ratio (mem_int mem) (mem_int max_mem)
    , txfSize = ratio (fromIntegral (costSizeBytes c)) (fromIntegral (cpMaxTxSize params))
    }
  where
    params = costParams c
    ExBudget cpu mem = costBudget c
    ExBudget max_cpu max_mem = cpMaxTxExUnits params
    ratio :: Int64 -> Int64 -> Double
    ratio a b = fromIntegral a / fromIntegral b

{-| One line for a cost, e.g.
@1,842 bytes (11.2% of max tx size), CPU 1,234,567 (0.0%), MEM 4,567 (0.0%), execution fee 360 lovelace [mainnet, ...]@ -}
prettyCost :: Cost -> Text
prettyCost c =
  prettySize params (costSizeBytes c)
    <> ", "
    <> prettyExecution c
    <> " ["
    <> cpSource params
    <> "]"
  where
    params = costParams c

{-| The execution units, their part of the maximum transaction execution
units, and the execution fee. -}
prettyExecution :: Cost -> Text
prettyExecution c =
  Text.concat
    [ "CPU "
    , commas (cpu_int cpu)
    , " ("
    , percent (txfCpu fr)
    , "), MEM "
    , commas (mem_int mem)
    , " ("
    , percent (txfMem fr)
    , "), execution fee "
    , commas (ceiling (executionFee (costParams c) (costBudget c)) :: Integer)
    , " lovelace"
    ]
  where
    ExBudget cpu mem = costBudget c
    fr = txFractions c

-- | The size of a script and its part of the maximum transaction size.
prettySize :: CostParams -> Int -> Text
prettySize params n =
  prettyBytes n
    <> " ("
    <> percent (fromIntegral n / fromIntegral (cpMaxTxSize params))
    <> " of max tx size)"

-- | A size in bytes, e.g. @1,842 bytes@.
prettyBytes :: Int -> Text
prettyBytes n = commas n <> " bytes"

prettyCostError :: CostError -> Text
prettyCostError = \case
  CostModelError err -> "incorrect cost model: " <> render err
  DecodeError err -> "the ledger rejects the script: " <> render err
  EvalError InvalidReturnValue _ ->
    "the result is not unit: the ledger accepts only a unit result from a PlutusV3 script"
  EvalError err logs ->
    "the script fails: "
      <> render err
      <> case logs of
        [] -> ""
        _ -> "\ntrace:\n" <> Text.intercalate "\n" logs
  where
    render :: Show e => e -> Text
    render = Text.pack . show

cpu_int :: ExCPU -> Int64
cpu_int (ExCPU n) = unSatInt n

mem_int :: ExMemory -> Int64
mem_int (ExMemory n) = unSatInt n

-- | Render a fraction as a percentage with one decimal.
percent :: Double -> Text
percent x = Text.pack (show (fromIntegral (round (x * 1000) :: Integer) / 10 :: Double)) <> "%"

-- | Render a number with thousands separators.
commas :: Integral n => n -> Text
commas n
  | n < 0 = "-" <> commas (negate n)
  | otherwise =
      Text.intercalate "," . reverse . fmap Text.reverse . Text.chunksOf 3 . Text.reverse $
        Text.pack (show (toInteger n))
