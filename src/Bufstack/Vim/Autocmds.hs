{-# LANGUAGE OverloadedStrings #-}
module Bufstack.Vim.Autocmds (
    addAutocmds
) where

import Neovim
import Neovim.API.String
import Control.Monad.Trans.Resource

import System.IO

import Bufstack.Core
import Bufstack.Util
import Bufstack.Error
import Bufstack.BufferOperations

bufLeaveAutocmd :: Bufstack -> Neovim env ()
bufLeaveAutocmd b = (vim_get_current_buffer `bindNvimEither` 
                    (\x -> pushBufferN b x >> return (Right ()))) >>= 
                        handleErrorN b

checkAutocmdRegister :: Bufstack -> Maybe (Either (Neovim env ()) ReleaseKey) -> Neovim env ()
checkAutocmdRegister b Nothing = checkAutocmdRegister b (Just . Left $ err)
                    where err = liftIO . hPutStrLn stderr $  errMsg
                          errMsg = "Failed to register autocmd (unknown error)"
checkAutocmdRegister _ (Just (Left err)) = err
checkAutocmdRegister b (Just (Right key)) = addReleaseKey b key

addAutocmds :: Bufstack -> Neovim env ()
addAutocmds b =
        let options = AutocmdOptions {
                        acmdPattern = "*",
                        acmdNested = False,
                        acmdGroup = Nothing
                        }
            in addAutocmd "BufLeave" options (bufLeaveAutocmd b) >>= 
                checkAutocmdRegister b

