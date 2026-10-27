{-| Script contexts for cost evaluations.

This module re-exports the 'ScriptContext' builder of plutus-ledger-api and
adds short forms for the usual cases.

The cost of a script depends on the size of its context. A built context
contains only what you add to it, but the context of a real transaction
also has the fee, the other inputs, the change outputs, etc. Thus the real
cost is usually higher. For example, the lock validator of the tutorial
costs 26,943,493 CPU with 'spendingContext' and 33,659,860 CPU in a real
unlock transaction on a devnet. Add to the context what the real
transaction contains, or keep a margin. -}
module Plinth.Cost.Context
  ( -- * Short forms
    spendingContext
  , mintingContext
  , contextArgs

    -- * The builder
  , module PlutusLedgerApi.Test.ScriptContextBuilder.Builder
  ) where

import PlutusLedgerApi.Test.ScriptContextBuilder.Builder
import PlutusLedgerApi.V3 (BuiltinData, Data, ScriptContext, Value)
import PlutusTx.IsData.Class (toData)

{-| The context of a spending script: one script input with the datum
(inline) and the redeemer. Add more with the builder, e.g.
@buildScriptContext (withSpendingScript r (withInlineDatum d) <> withSigner pkh)@. -}
spendingContext :: BuiltinData -> BuiltinData -> ScriptContext
spendingContext datum redeemer =
  buildScriptContext (withSpendingScript redeemer (withInlineDatum datum))

-- | The context of a minting script that mints the value.
mintingContext :: Value -> BuiltinData -> ScriptContext
mintingContext value redeemer = buildScriptContext (withMintingScript value redeemer)

-- | The arguments of a PlutusV3 script: the context only.
contextArgs :: ScriptContext -> [Data]
contextArgs ctx = [toData ctx]
