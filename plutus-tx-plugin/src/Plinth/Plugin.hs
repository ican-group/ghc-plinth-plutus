{-# LANGUAGE TemplateHaskellQuotes #-}

module Plinth.Plugin (plugin, pluginWith, plinthc) where

import PlutusTx.Options
import PlutusTx.Plugin.Boilerplate
import PlutusTx.Plugin.Common
import PlutusTx.Plugin.Unsupported
import PlutusTx.Plugin.Utils

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Either.Validation
import Data.Text qualified as Text
import GHC.Plugins qualified as GHC

-- | The plugin with no cost reporter: report-cost is an error.
plugin :: GHC.Plugin
plugin = pluginWith Nothing

{-| The plugin with a cost reporter. See Note [Cost reporter hook] in
PlutusTx.Plugin.Common. -}
pluginWith :: Maybe CostReporter -> GHC.Plugin
pluginWith reporter =
  GHC.defaultPlugin
    { GHC.driverPlugin = addFlagsAndExts
    , GHC.typeCheckResultAction = \cliOpts _modSummary env -> do
        opts <- case parsePluginOptions (removeBoilerplateOpts cliOpts) of
          Success o -> pure o
          Failure errs ->
            liftIO . GHC.throwGhcExceptionIO . GHC.CmdLineError . Text.unpack $
              renderPlinthParseErrors errs
        let maybeInjectAnchors =
              if opts ^. posPreserveSourceLocations then injectAnchors else pure
        maybeInjectAnchors env >>= injectUnsupportedMarkers >>= addInlineables
    , GHC.pluginRecompile = GHC.flagRecompile
    , GHC.installCoreToDos = installCorePlugin reporter 'plinthc
    }
