{-# LANGUAGE OverloadedStrings #-}
module Bufstack.Vim.Autocmds (
    addAutocmds
) where

import Control.Concurrent.STM
import Neovim
import Neovim.API.String
import Control.Monad.Trans.Resource

import Bufstack.Core
import Bufstack.Util
import Bufstack.BufferOperations

onError :: Neovim env ()
onError = undefined -- TODO

bufEnterAutocmd :: BufstackM ()
bufEnterAutocmd = vim_get_current_buffer' >>= pushBuffer

checkAutocmdRegister :: Maybe (Either (BufstackM ()) ReleaseKey) -> BufstackM ()
checkAutocmdRegister Nothing = onError
checkAutocmdRegister (Just (Left err)) = err >> onError
checkAutocmdRegister (Just (Right key)) = addReleaseKey key

addAutocmds :: BufstackM ()
addAutocmds =
        let options = AutocmdOptions {
                        acmdPattern = "*",
                        acmdNested = False,
                        acmdGroup = Nothing
                        }
            in addAutocmd "BufNext" options bufEnterAutocmd >>= 
                checkAutocmdRegister

