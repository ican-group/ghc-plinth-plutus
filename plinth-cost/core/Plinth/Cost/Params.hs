{-| Cost parameters of a Cardano network, pinned per protocol version.
See Note [Pinned cost models]. -}
module Plinth.Cost.Params
  ( CostParams (..)
  , mainnetParams
  , defaultTargetProtocolVersion
  , mainnetPlutusV3CostModel
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Plinth.Cost.Default (defaultTargetProtocolVersion)
import PlutusLedgerApi.Common (ExBudget (..), MajorProtocolVersion (..))

{- Note [Pinned cost models]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cost of a script has two inputs:

  1. the semantics variant of the builtins. The ledger selects it from the
     major protocol version (PV), see mkEvaluationContext in
     PlutusLedgerApi.V3.EvaluationContext.
  2. the cost model parameters and the prices. These are protocol
     parameters. Governance actions change them, also without a change of
     the PV. Thus the PV does not give them.

To prevent drift, each CostParams is a snapshot of the values of mainnet,
with its source (network and epoch). mainnetParams gives the snapshot for a
PV, or Nothing if we have no snapshot: then a tool reports the size only and
says that it has no cost model for this PV. A change of a snapshot is a
deliberate change: give it in the release notes.

Mainnet sets fewer PlutusV3 parameters (350) than the ledger expects (358):
the extra ones are for builtins that are not available yet. Like the node,
mkEvaluationContext gives them a very large cost. A script that uses only
available builtins does not use them.

To update a snapshot, get the parameters of the epoch, e.g.
  curl https://api.koios.rest/api/v1/epoch_params?_epoch_no=<N>
and copy cost_models.PlutusV3, price_mem, price_step, max_tx_ex_mem,
max_tx_ex_steps, max_tx_size, min_fee_a and min_fee_ref_script_cost_per_byte.
-}

-- | The parameters that set the cost of a script on a network.
data CostParams = CostParams
  { cpProtocolVersion :: MajorProtocolVersion
  -- ^ selects the semantics variant of the builtins
  , cpCostModelV3 :: [Int64]
  -- ^ the PlutusV3 cost model parameters, in the order of the ledger
  , cpPriceMem :: Rational
  -- ^ lovelace for each memory unit
  , cpPriceSteps :: Rational
  -- ^ lovelace for each CPU step
  , cpMaxTxExUnits :: ExBudget
  -- ^ the execution units that a transaction can use
  , cpMaxTxSize :: Integer
  -- ^ the size of a transaction in bytes
  , cpTxFeePerByte :: Integer
  -- ^ lovelace for each byte of a transaction (min_fee_a)
  , cpRefScriptCostPerByte :: Rational
  -- ^ lovelace for each byte of a reference script (first tier)
  , cpSource :: Text
  -- ^ where the values come from
  }
  deriving stock (Show)

{-| The pinned mainnet parameters for a PV, if we have them.
See Note [Pinned cost models]. -}
mainnetParams :: MajorProtocolVersion -> Maybe CostParams
mainnetParams = \case
  MajorProtocolVersion 10 -> Just (mainnet 10 "mainnet, epoch 643 (last epoch of PV 10)")
  MajorProtocolVersion 11 -> Just (mainnet 11 "mainnet, epochs 644-663 (PV 11, as of 2026-10-26)")
  _ -> Nothing
  where
    -- The values did not change from epoch 638 to epoch 663.
    mainnet pv src =
      CostParams
        { cpProtocolVersion = MajorProtocolVersion pv
        , cpCostModelV3 = mainnetPlutusV3CostModel
        , cpPriceMem = 577 / 10000
        , cpPriceSteps = 721 / 10000000
        , cpMaxTxExUnits = ExBudget 10000000000 16500000
        , cpMaxTxSize = 16384
        , cpTxFeePerByte = 44
        , cpRefScriptCostPerByte = 15
        , cpSource = src
        }

-- | The PlutusV3 cost model of mainnet, epochs 638-663 (350 parameters).
mainnetPlutusV3CostModel :: [Int64]
mainnetPlutusV3CostModel =
  [ 100788, 420, 1, 1, 1000, 173, 0, 1
  , 1000, 59957, 4, 1, 11183, 32, 201305, 8356
  , 4, 16000, 100, 16000, 100, 16000, 100, 16000
  , 100, 16000, 100, 16000, 100, 100, 100, 16000
  , 100, 94375, 32, 132994, 32, 61462, 4, 72010
  , 178, 0, 1, 22151, 32, 91189, 769, 4
  , 2, 85848, 123203, 7305, -900, 1716, 960, 57
  , 85848, 0, 1, 1, 1000, 42921, 4, 2
  , 30623, 28755, 75, 1, 898148, 27279, 1, 51775
  , 558, 1, 39184, 1000, 60594, 1, 141895, 32
  , 83150, 32, 15299, 32, 76049, 1, 13169, 4
  , 22100, 10, 28999, 74, 1, 28999, 74, 1
  , 43285, 552, 1, 44749, 541, 1, 33852, 32
  , 68246, 32, 72362, 32, 7243, 32, 7391, 32
  , 11546, 32, 85848, 123203, 7305, -900, 1716, 960
  , 57, 85848, 0, 1, 90434, 519, 0, 1
  , 74433, 32, 85848, 123203, 7305, -900, 1716, 960
  , 57, 85848, 0, 1, 1, 85848, 123203, 7305
  , -900, 1716, 960, 57, 85848, 0, 1, 955506
  , 213312, 0, 2, 270652, 22588, 4, 1457325, 64566
  , 4, 20467, 1, 4, 0, 141992, 32, 100788
  , 420, 1, 1, 81663, 32, 59498, 32, 20142
  , 32, 24588, 32, 20744, 32, 25933, 32, 24623
  , 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308
  , 10, 16000, 100, 16000, 100, 962335, 18, 2780678
  , 6, 442008, 1, 52538055, 3756, 18, 267929, 18
  , 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919
  , 12, 901022, 1, 166917843, 4307, 36, 284546, 36
  , 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273
  , 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310
  , 4, 1293828, 28716, 63, 0, 1, 1006041, 43623
  , 251, 0, 1, 100181, 726, 719, 0, 1
  , 100181, 726, 719, 0, 1, 100181, 726, 719
  , 0, 1, 107878, 680, 0, 1, 95336, 1
  , 281145, 18848, 0, 1, 180194, 159, 1, 1
  , 158519, 8942, 0, 1, 159378, 8813, 0, 1
  , 107490, 3298, 1, 106057, 655, 1, 1964219, 24520
  , 3, 607153, 231697, 53144, 0, 1, 116711, 1957
  , 4, 231883, 10, 1000, 24838, 7, 1, 232010
  , 32, 321837444, 25087669, 18, 617887431, 67302824, 36, 356924
  , 18413, 45, 21, 219951, 9444, 1, 1000, 172116
  , 183150, 6, 24, 21, 213283, 618401, 1998, 28258
  , 1, 1000, 38159, 2, 22, 1000, 95933, 1
  , 1, 11, 1000, 277577, 12, 21
  ]
