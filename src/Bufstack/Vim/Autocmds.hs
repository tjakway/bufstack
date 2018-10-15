{-# LANGUAGE OverloadedStrings #-}
module Bufstack.Vim.Autocmds (
    addAutocmds
) where

import Neovim
import Neovim.API.String
import Control.Monad.Trans.Resource

import Bufstack.Core
import Bufstack.Util
import Bufstack.Error
import Bufstack.BufferOperations

bufLeaveAutocmd :: BufstackM ()
bufLeaveAutocmd = (vim_get_current_buffer `bindNvimEither` (fmap Right . pushBuffer)) >>= 
                    handleErrorE

checkAutocmdRegister :: Maybe (Either (BufstackM ()) ReleaseKey) -> BufstackM ()
checkAutocmdRegister Nothing = checkAutocmdRegister (Just . Left $ err)
                    where err = handleError . ErrorMessage $ errMsg
                          errMsg = "Failed to register autocmd (unknown error)"
checkAutocmdRegister (Just (Left err)) = err
checkAutocmdRegister (Just (Right key)) = addReleaseKey key

addAutocmds :: BufstackM ()
addAutocmds =
        let options = AutocmdOptions {
                        acmdPattern = "*",
                        acmdNested = False,
                        acmdGroup = Nothing
                        }
            in addAutocmd "BufLeave" options bufLeaveAutocmd >>= 
                checkAutocmdRegister

