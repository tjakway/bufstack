{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.Class.Closeable where

import Neovim
import Neovim.API.String
import Control.Monad (foldM, mapM_, sequence_)

import Bufstack.Class.HasNumber
import Bufstack.Class.IsValid
import Bufstack.Util

class Closeable a where
        close :: a -> Neovim env (Either NeovimException ())

        close' :: a -> Neovim env ()
        close' x = let f (Right _) = Right ObjectNil
                       f (Left y) = Left y

                       closeNil :: Neovim env (Either NeovimException Object)
                       closeNil = (close x) >>= return . f

                        in errOnInvalidResult closeNil 

instance Closeable Buffer where
        close b = whenIsValid b $ getNumber b `bindNvimEither` (\n -> vim_command $ ":bwipeout " ++ show n)

instance Closeable Window where
        close b = whenIsValid b $ getNumber b `bindNvimEither` (\n -> vim_command $ ": " ++ show n ++ "close")


instance Closeable Tabpage where
        close b = whenIsValid b $ getNumber b `bindNvimEither` (\n -> vim_command $ ":tabclose " ++ show n)

closeAll :: Closeable a => [a] -> Neovim env (Either [NeovimException] ())
closeAll = fmap rev . foldM f startFold
    where f :: Closeable x => Either [NeovimException] () -> x -> Neovim env (Either [NeovimException] ())
          f (Right ()) thisElem = do
              n <- close thisElem
              return $ case n of Left r -> Left [r]
                                 Right _ -> Right ()

          f (Left errs) thisElem = do
              n <- close thisElem
              return $ case n of Left r -> Left $ r : errs
                                 Right _ -> Left errs

          startFold :: Either [NeovimException] ()
          startFold = return ()

          rev (Right ()) = Right ()
          rev (Left errs) = Left (reverse errs)

closeAll' :: Closeable a => ([NeovimException] -> Neovim env ()) -> [a] -> Neovim env ()
closeAll' onError xs = closeAll xs >>= h
    where h (Right ()) = return ()
          h (Left errs) = onError errs

