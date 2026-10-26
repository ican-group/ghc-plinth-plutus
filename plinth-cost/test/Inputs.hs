-- | Inputs of the test scripts. A separate module: the splices of Spec use them.
module Inputs (guess) where

import Plinth.Cost.Context (contextArgs, spendingContext)
import PlutusLedgerApi.Common (Data)
import PlutusTx (toBuiltinData)

-- | The arguments of the lock validator: the secret in the datum, the guess in the redeemer.
guess :: Integer -> Integer -> [Data]
guess secret g = contextArgs (spendingContext (toBuiltinData secret) (toBuiltinData g))
