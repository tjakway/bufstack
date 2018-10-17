module Bufstack.Init where

import Control.Monad.Trans.Resource
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
        return (bufstackData, resetBufstack)

resetBufstack :: Bufstack -> Neovim env ()
resetBufstack env@Bufstack{buffers=b, autocmds=a} =
        let stm = do
                writeTVar b []
                writeTVar a []
            in do
                autocmdKeys <- liftIO . readTVarIO $ a
                mapM_ release autocmdKeys
                liftIO . atomically $ stm
                addAutocmds env
