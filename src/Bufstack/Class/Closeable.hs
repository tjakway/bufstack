{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.Class.Closeable where

import Neovim
import Neovim.API.String
import Bufstack.Class.HasNumber
import Control.Monad (foldM, mapM_)

class Closeable a where
        close :: a -> Neovim env (Either NeovimException ())

        close' :: a -> Neovim env ()
        close' x = let f (Right _) = Right ObjectNil
                       f (Left y) = Left y

                       closeNil :: Neovim env (Either NeovimException Object)
                       closeNil = (close x) >>= return . f

                        in errOnInvalidResult closeNil 

instance Closeable Buffer where
        close b = getNumber b >>= (>>= (\n -> vim_command $ ":bd " ++ show n))

instance Closeable Window where
        close b = getNumber b >>= (>>= (\n -> vim_command $ ": " ++ show n ++ "quit"))


instance Closeable Tabpage where
        close b = vim_command (":tabclose " ++ (show . getNumber) b)
        close' b = vim_command' (":tabclose " ++ (show . getNumber) b)

closeAll :: Closeable a => [a] -> Neovim env (Either [NeovimException] ())
closeAll = g . foldM f (Right ())
    where f (Right ()) thisElem = case close thisElem of Left r -> [r]
                                                         Right _ -> Right ()
          f (Left errs) thisElem = case close thisElem of Left r -> Left $ r : errs
                                                          Right _ -> Left errs

          rev (Right ()) = Right ()
          rev (Left errs) = Left (reverse errs)

closeAll' :: Closeable a => [a] -> Neovim env ()
closeAll' = h . closeAll
    where h (Right ()) = ()
          h (Left errs) = mapM_ errOnInvalidResult errs

