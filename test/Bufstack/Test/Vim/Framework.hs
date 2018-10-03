{-|
- Contains setup/teardown code for running tests in neovim
-}
module Bufstack.Test.Vim.Framework where

import Test.Framework
import Test.Framework.Providers.HUnit

import Neovim
import Neovim.API.String
import Control.Concurrent.STM
import Control.Monad.Trans.Resource

import Bufstack.Core

cleanup :: Bufstack -> IO ()
cleanup Bufstack{buffers = b, autocmds = a} = do
        -- atomically clear bufstack fields
        cmds <- atomically $ do
            -- clear buffers
            writeTVar b []
            cmds <- readTVar a
            writeTVar a []
            return cmds

        -- release autocmd resource handles
        mapM_ release cmds
