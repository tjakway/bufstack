module Bufstack.Init where

import Control.Concurrent.STM
import Neovim

import Bufstack.Core
import Bufstack.Config.Type
import Bufstack.Vim.Autocmds

initBufstackData :: Config -> STM Bufstack
initBufstackData c = Bufstack c <$> newTVar [] <*> newTVar []

initBufstack :: Config -> Neovim env Bufstack
initBufstack c = initBufstack' >>= (\env -> addAutocmds env >> return env)
    where initBufstack' = liftIO . atomically . initBufstackData $ c

initBufstackIO :: Config -> IO (Bufstack, Bufstack -> Neovim env ())
initBufstackIO c = do
        bufstackData <- atomically . initBufstackData $ c
        return (bufstackData, addAutocmds)
