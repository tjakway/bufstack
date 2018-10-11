module Bufstack.Config.Default where

import Bufstack.Config.Type
import Bufstack.Error

defaultConfig :: Config
defaultConfig = Config False True defaultOnError 
