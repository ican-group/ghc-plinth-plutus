{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A small validator to test the cost functions.
module Scripts (lockScript, closedScript) where

import PlutusLedgerApi.V3
  (Datum (..), ScriptContext (..), ScriptInfo (..), getRedeemer)
import PlutusTx
import PlutusTx.Prelude qualified as Tx

-- The redeemer must be equal to the integer in the datum.
lockTyped :: ScriptContext -> Bool
lockTyped ctx =
  case scriptContextScriptInfo ctx of
    SpendingScript _ (Just (Datum datum)) ->
      case (fromBuiltinData datum, fromBuiltinData redeemer) of
        (Just secret, Just guess) ->
          Tx.traceIfFalse "wrong number" (guess Tx.== (secret :: Integer))
        _ -> Tx.traceError "datum or redeemer is not an integer"
    _ -> Tx.traceError "expected a spending script with a datum"
  where
    redeemer = getRedeemer (scriptContextRedeemer ctx)

lockUntyped :: BuiltinData -> Tx.BuiltinUnit
lockUntyped ctx = Tx.check (lockTyped (unsafeFromBuiltinData ctx))

lockScript :: CompiledCode (BuiltinData -> Tx.BuiltinUnit)
lockScript = $$(compile [|| lockUntyped ||])

-- A script without arguments.
closedScript :: CompiledCode Tx.BuiltinUnit
closedScript = $$(compile [|| Tx.check (2 Tx.+ 2 Tx.== (4 :: Integer)) ||])
