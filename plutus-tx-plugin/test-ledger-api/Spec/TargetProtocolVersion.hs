{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-protocol-version=11 #-}

{-| Evaluate a validator with the ledger API at the protocol version that it
targets. The Data decoders of 'makeIsDataIndexed' use 'case' on Data at
protocol version 12, in Plutus Core 1.2.0, and the ledger rejects that
version before protocol version 12. See Note [Target protocol version] in
PlutusTx.Compiler.Types. -}
module Spec.TargetProtocolVersion (tests) where

import PlutusLedgerApi.Common.Versions (MajorProtocolVersion, dijkstraPV, vanRossemPV)
import PlutusLedgerApi.Test.V3.EvaluationContext qualified as V3
import PlutusLedgerApi.V3 qualified as V3
import PlutusPrelude
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (BuiltinData, BuiltinUnit)
import PlutusTx.TH (compile)
import Spec.TargetProtocolVersion.Validator (shapeValidator, shapeValidatorPV12)

import Control.Monad.Writer (runWriterT)
import Data.List (isInfixOf)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "target-protocol-version"
    [ testGroup
        "compiled for PV11"
        [ expect True "evaluates at PV11" vanRossemPV shapeValidatorPV11 rect
        , expect True "evaluates at PV12" dijkstraPV shapeValidatorPV11 rect
        , expect False "rejects a bad shape at PV11" vanRossemPV shapeValidatorPV11 badRect
        ]
    , testGroup
        "compiled for PV12"
        [ expect True "evaluates at PV12" dijkstraPV shapeValidatorPV12 rect
        , testCase "fails at PV11: Plutus Core 1.2.0 is not available" $
            case evaluate vanRossemPV shapeValidatorPV12 rect of
              Left err
                | "PlutusCoreLanguageNotAvailableError" `isInfixOf` err -> pure ()
                | otherwise -> assertFailure ("wrong error: " <> err)
              Right _ -> assertFailure "evaluation succeeded"
        ]
    ]
  where
    rect = V3.Constr 2 [V3.I 3, V3.I 4]
    badRect = V3.Constr 2 [V3.I 3, V3.I 0]

shapeValidatorPV11 :: CompiledCode (BuiltinData -> BuiltinUnit)
shapeValidatorPV11 = $$(compile [||shapeValidator||])

evalCtx :: V3.EvaluationContext
evalCtx =
  fst . unsafeFromRight . runWriterT . V3.mkEvaluationContext $
    fmap snd V3.costModelParamsForTesting

-- | Evaluate the validator at a protocol version, and expect success or
-- failure. The failure can be at deserialisation or at evaluation.
expect
  :: Bool
  -> TestName
  -> MajorProtocolVersion
  -> CompiledCode (BuiltinData -> BuiltinUnit)
  -> V3.Data
  -> TestTree
expect success name pv code arg = testCase name $ case (success, evaluate pv code arg) of
  (True, Left err) -> assertFailure ("evaluation failed: " <> err)
  (False, Right _) -> assertFailure "evaluation succeeded"
  _ -> pure ()

evaluate
  :: MajorProtocolVersion
  -> CompiledCode (BuiltinData -> BuiltinUnit)
  -> V3.Data
  -> Either String V3.ExBudget
evaluate pv code arg = do
  script <- first show $ V3.deserialiseScript pv (V3.serialiseCompiledCode code)
  first show . snd $ V3.evaluateScriptCounting pv V3.Quiet evalCtx script arg
