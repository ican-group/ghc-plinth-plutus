{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-protocol-version=12 #-}

{-| A validator that decodes a type with sequential constructor indexes, for
"Spec.TargetProtocolVersion", and its build for major protocol version 12. -}
module Spec.TargetProtocolVersion.Validator where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData (makeIsDataIndexed, unsafeFromBuiltinData)
import PlutusTx.Prelude qualified as Tx
import PlutusTx.TH (compile)

data Shape = Point | Line Integer | Rect Integer Integer

makeIsDataIndexed ''Shape [('Point, 0), ('Line, 1), ('Rect, 2)]

shapeValidator :: Tx.BuiltinData -> Tx.BuiltinUnit
shapeValidator d = Tx.check Tx.$ case unsafeFromBuiltinData d of
  Point -> True
  Line l -> l Tx.> 0
  Rect w h -> w Tx.> 0 Tx.&& h Tx.> 0
{-# INLINEABLE shapeValidator #-}

shapeValidatorPV12 :: CompiledCode (Tx.BuiltinData -> Tx.BuiltinUnit)
shapeValidatorPV12 = $$(compile [||shapeValidator||])
