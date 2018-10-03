module Bufstack.Util (
    atomically,
    bindNvimEither,
    modifyBuffers_,
    modifyBuffers,
    modifyBuffersM,
    modifyBuffersM_,
    modifyBuffersMs,
    wrapNvimEither,
    addReleaseKey
) where

import qualified Control.Concurrent.STM as STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)
import Bufstack.Core

import Control.Monad.Trans.Resource

atomically :: STM.STM a -> Nvim.Neovim env a
atomically = Nvim.liftIO . STM.atomically

-- | bind twice
bindNvimEither :: Nvim.Neovim env (Either Nvim.NeovimException a) -> 
                    (a -> Nvim.Neovim env (Either Nvim.NeovimException b)) -> 
                    Nvim.Neovim env (Either Nvim.NeovimException b)
bindNvimEither x y = let f (Right r) = y r
                         f (Left l) = return . Left $ l
                           in x >>= f

modifyBuffers_ :: Bufstack -> ([Nvim.Buffer] -> [Nvim.Buffer]) -> STM.STM ()
modifyBuffers_ Bufstack {buffers= bufs} = STM.modifyTVar' bufs


modifyBuffers :: Bufstack -> 
                ([Nvim.Buffer] -> [Nvim.Buffer]) -> 
                STM.STM [Nvim.Buffer]
modifyBuffers b@Bufstack {buffers= bufs} f = 
        modifyBuffers_ b f >> STM.readTVar bufs


modifyBuffersMImpl :: (Bufstack -> 
                    ([Nvim.Buffer] -> [Nvim.Buffer]) -> 
                    STM.STM a) -> ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM a
modifyBuffersMImpl g f = let m x = atomically $ g x f
                            in Nvim.ask >>= m

modifyBuffersM_ :: ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM ()
modifyBuffersM_ = modifyBuffersMImpl modifyBuffers_


modifyBuffersM :: ([Nvim.Buffer] -> [Nvim.Buffer]) -> BufstackM [Nvim.Buffer]
modifyBuffersM = modifyBuffersMImpl modifyBuffers

modifyBuffersMs :: ([Nvim.Buffer] -> (a, [Nvim.Buffer])) -> BufstackM a
modifyBuffersMs f = let x (Bufstack {buffers = bufs}) = atomically $ stateTVar bufs f
                        in Nvim.ask >>= x

wrapNvimEither :: Either Nvim.NeovimException (Nvim.Neovim env a) -> Nvim.Neovim env (Either Nvim.NeovimException a)
wrapNvimEither (Left x) = return . Left $ x
wrapNvimEither (Right x) = x >>= (\a -> return . Right $ a)

-- prepend this key to the list of release keys
addReleaseKey :: ReleaseKey -> BufstackM ()
addReleaseKey k = Nvim.ask >>= (\(Bufstack{autocmds = releaseKeys}) -> 
                        atomically $ STM.modifyTVar' releaseKeys (k :))

-- verbatim from http://hackage.haskell.org/package/stm-2.5.0.0/docs/src/Control.Concurrent.STM.TVar.html#stateTVar
-- | Like 'modifyTVar'' but the function is a simple state transition that can
-- return a side value which is passed on as the result of the 'STM'.
--
-- @since 2.5.0
stateTVar :: STM.TVar s -> (s -> (a, s)) -> STM.STM a
stateTVar var f = do
   s <- STM.readTVar var
   let (a, s') = f s -- since we destructure this, we are strict in f
   STM.writeTVar var s'
   return a
{-# INLINE stateTVar #-}
