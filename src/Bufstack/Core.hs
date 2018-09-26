module Bufstack.Core where

import Control.Concurrent.STM
import Neovim.API.String

data Config = Config

data Bufstack =
    Bufstack {
        config :: Config,
        buffers :: TVar [Buffer]
        }

initBufstack :: Config -> STM Bufstack
initBufstack c = Bufstack c <$> newTVar []
