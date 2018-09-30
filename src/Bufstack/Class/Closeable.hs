module Bufstack.Class.Closeable where

import Neovim
import Neovim.API.String
import Bufstack.Close.HasNumber
import Control.Monad (foldM, mapM_)

class NvimCloseable a where
        close :: a -> Neovim env (Either NeovimException ())
        close' :: a -> Neovim env ()

instance NvimCloseable Buffer where
        close b = vim_command (":bd " ++ show . getNumber b)
        close' b = vim_command' (":bd " ++ show . getNumber b)

instance NvimCloseable Window where
        close b = vim_command (":" ++ show . getNumber b ++ "quit")
        close' b = vim_command' (":" ++ show . getNumber b ++ "quit")


instance NvimCloseable Tabpage where
        close b = vim_command (":tabclose " ++ show . getNumber b)
        close' b = vim_command' (":tabclose " ++ show . getNumber b)

closeAll :: NvimCloseable a => [a] -> Neovim env (Either [NeovimException] ())
closeAll = g . foldM f (Right ())
    where f (Right ()) thisElem = case close thisElem of Left r -> [r]
                                                         Right _ -> Right ()
          f (Left errs) thisElem = case close thisElem of Left r -> Left $ r : errs
                                                          Right _ -> Left errs

          rev (Right ()) = Right ()
          rev (Left errs) = Left (reverse errs)

closeAll' :: NvimCloseable a => [a] -> Neovim env ()
closeAll' = h . closeAll
    where h (Right ()) = ()
          h (Left errs) = mapM_ errOnInvalidResult errs

