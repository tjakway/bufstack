module Bufstack.Core where

import Control.Concurrent.STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)

data Config = 
    Config {
        --to indicate their position on the stack
        renameBuffers :: Bool
    }


defaultConfig :: Config
defaultConfig = Config False

data Bufstack =
    Bufstack {
        config :: Config,
        buffers :: TVar [Nvim.Buffer]
        }

type BufstackM a = Nvim.Neovim Bufstack a

initBufstack :: Config -> STM Bufstack
initBufstack c = Bufstack c <$> newTVar []

discardBadBuffers :: BufstackM ()
discardBadBuffers = 
        Nvim.ask >>= (\Bufstack {buffers= bufs} ->
            (Nvim.liftIO . readTVarIO $ bufs) >>= 
            filterM (Nvim.buffer_is_valid') >>=
            (Nvim.liftIO . atomically . writeTVar bufs))

