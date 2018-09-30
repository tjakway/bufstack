{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.Class.Closeable where

import Neovim
import Neovim.API.String
import Control.Monad (foldM, mapM_)

import Bufstack.Class.HasNumber
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
        close b = getNumber b >>= (\n -> vim_command $ ":bd " ++ show n)

instance Closeable Window where
        close b = getNumber b >>= (\n -> vim_command $ ": " ++ show n ++ "quit")


instance Closeable Tabpage where
        close b = getNumber b >>= (\n -> vim_command $ ":tabclose " ++ show n)

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

closeAll' :: Closeable a => [a] -> Neovim env ()
closeAll' = h . closeAll
    where h (Right ()) = ()
          h (Left errs) = mapM_ errOnInvalidResult errs

