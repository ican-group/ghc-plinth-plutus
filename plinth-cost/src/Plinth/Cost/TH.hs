{-| Report the cost of a script at compile time.

@
-- Scripts.hs
lockScript :: CompiledCode (BuiltinData -> BuiltinUnit)
lockScript = $$(compile [|| lockUntyped ||])

-- Costs.hs
import Plinth.Cost.Context
import Plinth.Cost.TH
import Scripts (lockScript)

\$(reportCost "lock, good guess" lockScript
    (contextArgs (spendingContext (toBuiltinData (42 :: Integer)) (toBuiltinData (42 :: Integer)))))

\$(assertCost "lock, good guess" (noLimits {maxCpu = Just 50_000_000}) lockScript ...)
@

The script must come from another module (Template Haskell stage
restriction). The splices use the pinned mainnet parameters of
'defaultTargetProtocolVersion'. For another PV, use the @...With@ forms.
See Note [Pinned cost models] in "Plinth.Cost.Params". -}
module Plinth.Cost.TH
  ( reportCost
  , reportCostWith
  , Limits (..)
  , noLimits
  , assertCost
  , assertCostWith
  ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.SatInt (unSatInt)
import Data.Text qualified as Text
import Language.Haskell.TH (Dec, Q, reportError, reportWarning)
import Plinth.Cost
import PlutusLedgerApi.Common (Data, ExBudget (..), ExCPU (..), ExMemory (..))
import PlutusTx.Code (CompiledCode)

{-| Evaluate the script on the arguments and report the cost as a warning.
GHC has no information level for splices: with -Werror, use 'assertCost'. -}
reportCost :: String -> CompiledCode a -> [Data] -> Q [Dec]
reportCost = reportCostWith default_params

-- | 'reportCost' with the given parameters.
reportCostWith :: CostParams -> String -> CompiledCode a -> [Data] -> Q [Dec]
reportCostWith params name code args = do
  case evalCost params code args of
    Right c -> reportWarning (header name <> Text.unpack (prettyCost c))
    Left err -> reportError (header name <> Text.unpack (prettyCostError err))
  pure []

-- | Upper limits for 'assertCost'. 'Nothing' is no limit.
data Limits = Limits
  { maxCpu :: Maybe Int64
  , maxMem :: Maybe Int64
  , maxSize :: Maybe Int
  -- ^ bytes
  }
  deriving stock (Show)

noLimits :: Limits
noLimits = Limits Nothing Nothing Nothing

{-| Evaluate the script on the arguments. Fail the compilation if the
evaluation fails or if the cost is over a limit. Report nothing else. -}
assertCost :: String -> Limits -> CompiledCode a -> [Data] -> Q [Dec]
assertCost = assertCostWith default_params

-- | 'assertCost' with the given parameters.
assertCostWith :: CostParams -> String -> Limits -> CompiledCode a -> [Data] -> Q [Dec]
assertCostWith params name limits code args = do
  case evalCost params code args of
    Left err -> reportError (header name <> Text.unpack (prettyCostError err))
    Right c ->
      case over_limits c of
        [] -> pure ()
        overs ->
          reportError $
            header name
              <> "over the limits: "
              <> unwords overs
              <> "\n"
              <> Text.unpack (prettyCost c)
  pure []
  where
    over_limits c =
      let ExBudget (ExCPU cpu) (ExMemory mem) = costBudget c
       in catMaybes
            [ check "CPU" (unSatInt cpu) =<< maxCpu limits
            , check "MEM" (unSatInt mem) =<< maxMem limits
            , check "size" (costSizeBytes c) =<< maxSize limits
            ]
    check :: (Ord n, Show n) => String -> n -> n -> Maybe String
    check what actual limit
      | actual > limit = Just (what <> " " <> show actual <> " > " <> show limit)
      | otherwise = Nothing

header :: String -> String
header name = "[Plinth cost] " <> name <> ": "

default_params :: CostParams
default_params = case mainnetParams defaultTargetProtocolVersion of
  Just p -> p
  Nothing -> error "Plinth.Cost.TH: no pinned parameters for the default PV"
