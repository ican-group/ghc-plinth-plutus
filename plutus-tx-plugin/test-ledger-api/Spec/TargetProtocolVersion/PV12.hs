{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-protocol-version=12 #-}

-- | 'shapeValidator' compiled for major protocol version 12.
module Spec.TargetProtocolVersion.PV12 where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (BuiltinData, BuiltinUnit)
import PlutusTx.TH (compile)
import Spec.TargetProtocolVersion.Validator (shapeValidator)

shapeValidatorPV12 :: CompiledCode (BuiltinData -> BuiltinUnit)
shapeValidatorPV12 = $$(compile [||shapeValidator||])
