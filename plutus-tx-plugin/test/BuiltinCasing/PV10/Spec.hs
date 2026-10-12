{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-protocol-version=10 #-}

{-| The examples of "BuiltinCasing.Spec", compiled for major protocol version
10: no 'case' on built-in types, but the builtins that do the same
(ifThenElse, chooseUnit, fstPair, headList, unConstrData, ...).
See Note [Target protocol version] in PlutusTx.Compiler.Types. -}
module BuiltinCasing.PV10.Spec where

import BuiltinCasing.Spec (addPair, assert, dataFields, forceUnit, head, integerABC)
import PlutusTx (compile)
import PlutusTx.Prelude (pure, ($), (.))
import PlutusTx.Test (goldenUPlcReadable)
import System.FilePath ((</>))
import Test.Tasty.Extras (TestNested, testNested, testNestedGhc)

tests :: TestNested
tests =
  testNested ("BuiltinCasing" </> "PV10")
    . pure
    $ testNestedGhc
      [ goldenUPlcReadable "assert" $$(compile [||assert||])
      , goldenUPlcReadable "forceUnit" $$(compile [||forceUnit||])
      , goldenUPlcReadable "addPair" $$(compile [||addPair||])
      , goldenUPlcReadable "integerABC" $$(compile [||integerABC||])
      , goldenUPlcReadable "head" $$(compile [||head||])
      , goldenUPlcReadable "dataFields" $$(compile [||dataFields||])
      ]
