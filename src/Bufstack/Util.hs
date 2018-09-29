module Bufstack.Util where

import qualified Control.Concurrent.STM as STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)
import Bufstack.Core

atomically :: STM.STM a -> Nvim.Neovim env a
atomically = Nvim.liftIO . STM.atomically


modifyBuffers_ :: Bufstack -> ([Nvim.Buffer] -> [Nvim.Buffer]) -> STM.STM ()
modifyBuffers_ Bufstack {buffers= bufs} = STM.modifyTVar' bufs


modifyBuffers :: Bufstack -> 
                ([Nvim.Buffer] -> [Nvim.Buffer]) -> 
                STM.STM [Nvim.Buffer]
modifyBuffers Bufstack {buffers= bufs} f = 
        modifyBuffers_ bufs f >> readTVar bufs


modifyBuffersMImpl :: (Bufstack -> 
                    ([Nvim.Buffer] -> [Nvim.Buffer]) -> 
                    STM.STM a) -> ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM ()
modifyBuffersMImpl g f = 
        let m x = atomically $ g x f
            in Nvim.ask >>= m

modifyBuffersM_ :: ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM ()
modifyBuffersM_ f = 
        let m x = atomically $ modifyBuffers_ x f
            in Nvim.ask >>= m


modifyBuffersM :: ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM [Nvim.Buffer]
modifyBuffersM f = 
        let m x = atomically $ modifyBuffers x f
            in Nvim.ask >>= m
