{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Maybe (isNothing)
import Inputs (guess)
import Plinth.Cost
import Plinth.Cost.TH
import PlutusLedgerApi.Common (ExBudget (..), MajorProtocolVersion (..))
import Scripts (closedScript, lockScript)
import Test.Tasty
import Test.Tasty.HUnit

-- Compile-time checks: the build fails if the cost is over the limits.
$(assertCost "lock, good guess" noLimits {maxSize = Just 4000} lockScript (guess 42 42))

$(reportCost "lock, good guess" lockScript (guess 42 42))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "plinth-cost"
    [ testGroup "pinned parameters" params_tests
    , testGroup "evaluation" eval_tests
    , testGroup "fees" fee_tests
    ]

pv11 :: CostParams
pv11 = case mainnetParams (MajorProtocolVersion 11) of
  Just p -> p
  Nothing -> error "no PV 11 parameters"

params_tests :: [TestTree]
params_tests =
  [ testCase "PV 10 and PV 11 have the 350 parameters of mainnet" $
      fmap (length . cpCostModelV3) . mainnetParams . MajorProtocolVersion <$> [10, 11]
        @?= [Just 350, Just 350]
  , testCase "the default PV has parameters" $
      assertBool "no parameters" (not (isNothing (mainnetParams defaultTargetProtocolVersion)))
  , testCase "no parameters for PV 12" $
      assertBool "PV 12 has parameters" (isNothing (mainnetParams (MajorProtocolVersion 12)))
  ]

eval_tests :: [TestTree]
eval_tests =
  [ testCase "lock: good guess (pinned budget)" $
      case evalCost pv11 lockScript (guess 42 42) of
        Left err -> assertFailure (show err)
        Right c -> do
          costBudget c @?= expected_lock_budget
          costSizeBytes c @?= scriptSize lockScript
  , testCase "lock: wrong guess fails with the trace" $
      case evalCost pv11 lockScript (guess 42 7) of
        -- PT5 is the error code of 'check'.
        Left (EvalError _ logs) -> logs @?= ["wrong number", "PT5"]
        other -> assertFailure ("expected an evaluation error, got " <> show other)
  , testCase "a closed script evaluates with no arguments" $
      case evalCost pv11 closedScript [] of
        Left err -> assertFailure (show err)
        Right c -> assertBool "zero budget" (costBudget c /= ExBudget 0 0)
  ]

{-| The cost of the good guess with the PV 11 parameters. A change of this
value is a change of the compiler or of the cost model: check it. -}
expected_lock_budget :: ExBudget
expected_lock_budget = ExBudget 26943493 112717

fee_tests :: [TestTree]
fee_tests =
  [ testCase "execution fee" $
      executionFee pv11 (ExBudget 10000000 10000) @?= 721 + 577
  , testCase "size fee" $ txSizeFee pv11 1000 @?= 44000
  , testCase "reference script fee, first tier" $ refScriptFee pv11 1000 @?= 15000
  , testCase "reference script fee, second tier" $
      refScriptFee pv11 (25600 + 1000) @?= 25600 * 15 + 18000
  ]
