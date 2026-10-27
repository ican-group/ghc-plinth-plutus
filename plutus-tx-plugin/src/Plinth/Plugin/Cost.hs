{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The cost report of the plugin (option report-cost). Only
Plinth.Plugin.Static imports this module: see Note [Cost reporter hook] in
PlutusTx.Plugin.Common. -}
module Plinth.Plugin.Cost (reportCost) where

import Plinth.Cost qualified as Cost
import PlutusLedgerApi.Common (MajorProtocolVersion (..))
import PlutusTx.Code (CompiledCode, CompiledCodeIn (SerializedCode))
import PlutusTx.Compiler.Utils (getVarSourceSpan)
import PlutusTx.Options (PluginOptions (..))
import PlutusTx.Plugin.Common (CostReporter, PluginCtx (..))
import UntypedPlutusCore qualified as UPLC

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import Data.Text qualified as Text
import GHC.Data.Strict qualified as Strict
import GHC.Plugins qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Utils.Logger qualified as GHC

{- Note [Reporting the cost]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With the option report-cost (-fplinth-report-cost), the plugin reports the
cost of each compiled script as a warning at the definition that contains
the 'compile' splice:

  Scripts.hs:28:1: warning:
      [Plinth cost] lockScript: 1,842 bytes (11.2% of max tx size), 412 AST nodes [...]

The plugin makes the CompiledCode value of the script and uses the functions
of Plinth.Cost, thus the numbers are the same as at run time. The parameters
are the pinned mainnet parameters of the target protocol version, see
Note [Pinned cost models] in Plinth.Cost.Params. For a PV with no pinned
parameters, the plugin reports the size only.

A script that takes arguments has no cost without them: the plugin reports
its size only. For a script that takes no arguments (its type is not a
function type), the plugin also evaluates it and reports the execution units
and the fee.

The plugin writes the warning to the logger directly. Thus -Werror does not
make it an error.
-}

-- | See Note [Reporting the cost]
reportCost :: CostReporter
reportCost codeTy uplcP plcBytes covIdx = do
  opts <- asks pcOpts
  mdef <- asks pcCurrentDef
  logger <- lift . lift $ GHC.getLogger
  let code = SerializedCode plcBytes Nothing covIdx :: CompiledCode ()
      pv = MajorProtocolVersion (_posTargetProtocolVersion opts)
      size = Cost.scriptSize code
      nodes = Text.pack (show (UPLC.unAstSize (UPLC.programAstSize uplcP))) <> " AST nodes"
      -- Always in this order: size, AST nodes, execution, source.
      -- An evaluation error goes on the next line.
      what = case Cost.mainnetParams pv of
        Nothing ->
          Cost.prettyBytes size
            <> ", "
            <> nodes
            <> "; no pinned cost model for PV "
            <> Text.pack (show (getMajorProtocolVersion pv))
        Just params ->
          let line parts = Text.intercalate ", " parts <> " [" <> Cost.cpSource params <> "]"
              size_nodes = [Cost.prettySize params size, nodes]
           in if GHC.isFunTy codeTy
                then line size_nodes
                else case Cost.evalCost params code [] of
                  Right c -> line (size_nodes <> [Cost.prettyExecution c])
                  Left err -> line size_nodes <> "\n" <> Cost.prettyCostError err
      name = maybe "" (\b -> GHC.getOccString b <> ": ") mdef
      loc = case getVarSourceSpan =<< mdef of
        Just ss -> GHC.RealSrcSpan ss Strict.Nothing
        Nothing -> GHC.noSrcSpan
#if __GLASGOW_HASKELL__ >= 908
      msg_class =
        GHC.MCDiagnostic GHC.SevWarning (GHC.ResolvedDiagnosticReason GHC.WarningWithoutFlag) Nothing
#else
      msg_class = GHC.MCDiagnostic GHC.SevWarning GHC.WarningWithoutFlag Nothing
#endif
  liftIO . GHC.logMsg logger msg_class loc $
    GHC.vcat (GHC.text . Text.unpack <$> Text.lines ("[Plinth cost] " <> Text.pack name <> what))
