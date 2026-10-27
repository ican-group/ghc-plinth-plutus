{-| The default target protocol version. The Plinth plugin imports this
module, thus it has only small imports: see Note [Cost reporter hook] in
plutus-tx-plugin:PlutusTx.Plugin.Common. -}
module Plinth.Cost.Default (defaultTargetProtocolVersion) where

import PlutusLedgerApi.Common (MajorProtocolVersion (..))

{-| The PV that the Plinth compiler targets by default. See Note [Target
protocol version] in PlutusTx.Compiler.Types. -}
defaultTargetProtocolVersion :: MajorProtocolVersion
defaultTargetProtocolVersion = MajorProtocolVersion 11
