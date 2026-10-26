{-| The plugin that plinthc registers: 'Plinth.Plugin.plugin' with the cost
report. Only the plinthc driver imports this module: see
Note [Cost reporter hook] in PlutusTx.Plugin.Common. -}
module Plinth.Plugin.Static (plugin) where

import Plinth.Plugin (pluginWith)
import Plinth.Plugin.Cost (reportCost)

import GHC.Plugins qualified as GHC

plugin :: GHC.Plugin
plugin = pluginWith (Just reportCost)
