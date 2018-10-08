module Bufstack.Util (
    atomically,
    bindNvimEither,
    fmapNvimEither,
    modifyBuffers_,
    modifyBuffers,
    modifyBuffersM,
    modifyBuffersM_,
    modifyBuffersMs,
    wrapNvimEither,
    addReleaseKey,
    newBuffer,
    newBuffer',
    getNumberedBuffers,
    getNumberedBuffers'
) where

import qualified Control.Concurrent.STM as STM
import Neovim
import Neovim.API.String

import Control.Monad
import Data.List (sortBy, nub)
import Data.Function (on)

import Control.Monad.Trans.Resource

import Bufstack.Core
import Bufstack.Error
import Bufstack.Class.HasNumber

atomically :: STM.STM a -> Neovim env a
atomically = liftIO . STM.atomically

-- | bind twice
bindNvimEither :: Neovim env (Either NeovimException a) -> 
                    (a -> Neovim env (Either NeovimException b)) -> 
                    Neovim env (Either NeovimException b)
bindNvimEither x y = let f (Right r) = y r
                         f (Left l) = return . Left $ l
                           in x >>= f

-- | bindNvimEither and discard result
bindNvimEither_ :: Neovim env (Either NeovimException a) -> 
                    (Neovim env (Either NeovimException b)) -> 
                    Neovim env (Either NeovimException b)
bindNvimEither_ x y = bindNvimEither x (\_ -> y)


-- | fmap twice
fmapNvimEither :: (a -> b) -> 
                    Neovim env (Either NeovimException a) -> 
                    Neovim env (Either NeovimException b)
fmapNvimEither x y = (fmap (fmap x)) y

modifyBuffers_ :: Bufstack -> ([Buffer] -> [Buffer]) -> STM.STM ()
modifyBuffers_ Bufstack {buffers= bufs} = STM.modifyTVar' bufs


modifyBuffers :: Bufstack -> 
                ([Buffer] -> [Buffer]) -> 
                STM.STM [Buffer]
modifyBuffers b@Bufstack {buffers= bufs} f = 
        modifyBuffers_ b f >> STM.readTVar bufs


modifyBuffersMImpl :: (Bufstack -> 
                    ([Buffer] -> [Buffer]) -> 
                    STM.STM a) -> ([Buffer] -> [Buffer]) -> BufstackM a
modifyBuffersMImpl g f = let m x = atomically $ g x f
                            in ask >>= m

modifyBuffersM_ :: ([Buffer] -> [Buffer]) -> BufstackM ()
modifyBuffersM_ = modifyBuffersMImpl modifyBuffers_


modifyBuffersM :: ([Buffer] -> [Buffer]) -> BufstackM [Buffer]
modifyBuffersM = modifyBuffersMImpl modifyBuffers

modifyBuffersMs :: ([Buffer] -> (a, [Buffer])) -> BufstackM a
modifyBuffersMs f = let x (Bufstack {buffers = bufs}) = atomically $ stateTVar bufs f
                        in ask >>= x

wrapNvimEither :: Either NeovimException (Neovim env a) -> Neovim env (Either NeovimException a)
wrapNvimEither (Left x) = return . Left $ x
wrapNvimEither (Right x) = x >>= (\a -> return . Right $ a)

-- prepend this key to the list of release keys
addReleaseKey :: ReleaseKey -> BufstackM ()
addReleaseKey k = ask >>= (\(Bufstack{autocmds = releaseKeys}) -> 
                        atomically $ STM.modifyTVar' releaseKeys (k :))

newBuffer :: Neovim env (Either NeovimException Buffer)
newBuffer = vim_command "new" `bindNvimEither_` 
                    vim_get_current_buffer


newBuffer' :: Neovim env Buffer
newBuffer' = newBuffer >>= \x -> 
                case x of Left y -> errOnInvalidResult (fmapNvimEither toObject newBuffer)
                          Right y -> return y


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

getNumberedBuffers :: BufstackMEither [(Buffer, NvimNumber)]
getNumberedBuffers =
        let accEithers acc (buf, getNumF) = do
                        x <- getNumF
                        return $ case (acc, x) of (Left errs, Left e) -> Left (e : errs)
                                                  (Left errs, _) -> Left errs
                                                  (Right ys, Right y) -> Right ((buf, y) : ys)
                                                  (_, Left e) -> Left [e]

            joinErrors x = let e = pretty "Accumulated errors from getNumber applied to results from nvim_list_bufs: "
                               in ErrorMessage (e <> (pretty . show . reverse $ x))
            mapLeft f (Left x) = Left (f x)
            mapLeft _ (Right y) = Right y

            doSort (Right xs) = Right . sortBy (compare `on` snd) $ xs
            doSort (Left xs) = Left xs

            in nvim_list_bufs `bindNvimEither` 
                    (fmap (doSort . mapLeft joinErrors) . 
                        foldM accEithers (Right []) . 
                        map (\x -> (x, getNumber x)) . 
                        nub)

getNumberedBuffers' :: BufstackM [(Buffer, NvimNumber)]
getNumberedBuffers' = handleErrorWithDefault [] getNumberedBuffers
