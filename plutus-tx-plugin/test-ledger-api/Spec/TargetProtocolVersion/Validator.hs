{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-| A validator that decodes a type with sequential constructor indexes, for
"Spec.TargetProtocolVersion". -}
module Spec.TargetProtocolVersion.Validator where

import PlutusTx.IsData (makeIsDataIndexed, unsafeFromBuiltinData)
import PlutusTx.Prelude qualified as Tx

data Shape = Point | Line Integer | Rect Integer Integer

makeIsDataIndexed ''Shape [('Point, 0), ('Line, 1), ('Rect, 2)]

shapeValidator :: Tx.BuiltinData -> Tx.BuiltinUnit
shapeValidator d = Tx.check Tx.$ case unsafeFromBuiltinData d of
  Point -> True
  Line l -> l Tx.> 0
  Rect w h -> w Tx.> 0 Tx.&& h Tx.> 0
{-# INLINEABLE shapeValidator #-}
