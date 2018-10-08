{-# LANGUAGE ExistentialQuantification #-}
module Bufstack.Core where

import Control.Concurrent.STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)
import Control.Monad.Trans.Resource

-- | datatype to hold instances of the Doc typeclass
data DocData = forall a. Nvim.Doc a => DocData a

data Config env = 
    Config {
        --to indicate their position on the stack
        renameBuffers :: Bool,
        onErrorF :: DocData -> Nvim.Neovim env ()
    }


defaultConfig :: Config
defaultConfig = Config False

data Bufstack =
    Bufstack {
        config :: Config,
        buffers :: TVar [Nvim.Buffer],
        -- registered autocmds (addAutocmd returns a ReleaseKey)
        autocmds :: TVar [ReleaseKey]
        }

type BufstackM a = Nvim.Neovim Bufstack a
type BufstackMEither a = Nvim.Neovim Bufstack (Either Nvim.NeovimException a)

initBufstack :: Config -> STM Bufstack
initBufstack c = Bufstack c <$> newTVar [] <*> newTVar []

initBufstackIO :: Config -> IO Bufstack
initBufstackIO = atomically . initBufstack

discardBadBuffers :: BufstackM ()
discardBadBuffers = 
        Nvim.ask >>= (\Bufstack {buffers= bufs} ->
            (Nvim.liftIO . readTVarIO $ bufs) >>= 
            filterM (Nvim.buffer_is_valid') >>=
            (Nvim.liftIO . atomically . writeTVar bufs))

getBuffers :: BufstackM [Nvim.Buffer]
getBuffers = Nvim.ask >>= (\(Bufstack{buffers = buf}) -> 
                Nvim.liftIO . readTVarIO $ buf)

onError :: Nvim.Doc a => Bufstack -> a -> Nvim.Neovim env ()
onError = errF
        where errF = f . onErrorF . config
              f (DocData a) = a
