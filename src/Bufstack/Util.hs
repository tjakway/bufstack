module Bufstack.Util where

import qualified Control.Concurrent.STM as STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)
import Bufstack.Core

atomically :: STM.STM a -> Nvim.Neovim env a
atomically = Nvim.liftIO . STM.atomically


modifyBuffers :: Bufstack -> ([Nvim.Buffer] -> [Nvim.Buffer]) -> STM.STM ()
modifyBuffers Bufstack {buffers= bufs} = STM.modifyTVar' bufs

modifyBuffersM :: ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM ()
modifyBuffersM f = 
        let m x = atomically $ modifyBuffers x f
            in Nvim.ask >>= m
