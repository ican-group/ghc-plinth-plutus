{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Plinth.Plugin (plugin, plinthc) where

import PlutusTx.Options
import PlutusTx.Plugin.Boilerplate
import PlutusTx.Plugin.Common
import PlutusTx.Plugin.Unsupported
import PlutusTx.Plugin.Utils

import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Either.Validation
import GHC.Plugins qualified as GHC

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.driverPlugin = addFlagsAndExts
    , GHC.typeCheckResultAction = \cliOpts _modSummary env -> do
        opts <- case parsePluginOptions (removeBoilerplateOpts cliOpts) of
          Success o -> pure o
          Failure errs -> liftIO $ throwIO errs
        let maybeInjectAnchors =
              if opts ^. posPreserveSourceLocations then injectAnchors else pure
        -- See Note [INLINABLE pragmas by the plugin] in
        -- PlutusTx.Plugin.Boilerplate.
        maybeInjectAnchors env
          >>= injectUnsupportedMarkers
#if __GLASGOW_HASKELL__ < 912
          >>= addInlineables
#endif
    , GHC.pluginRecompile = GHC.flagRecompile
    , GHC.installCoreToDos = installCorePlugin 'plinthc
    }
