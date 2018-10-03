{-|
- Contains setup/teardown code for running tests in neovim
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.Test.Vim.Framework where

import Test.Framework
import Test.Framework.Providers.HUnit

import Neovim
import Neovim.API.String
import Control.Concurrent.STM
import Control.Monad.Trans.Resource

import Bufstack.Core

import Bufstack.Test.Vim.Utils

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

checkTestEnvironment :: BufstackM ()
checkTestEnvironment = checkLengths
        where errMsg :: String -> Int -> String
              errMsg obj num = "Expected 1 " ++ obj ++ " but got: " ++ show num

              objects :: Neovim env [(Int, String)]
              objects =
                  (length <$> vim_get_buffers') >>= 
                    \numBuffers -> 
                    (length <$> vim_get_windows') >>=
                    \numWindows ->
                    (length <$> vim_get_tabpages') >>=
                    \numTabpages ->
                        return [(numBuffers, "buffer"),
                                (numWindows, "window"),
                                (numTabpages, "tabpage")]

              checkLengths :: Neovim env ()
              checkLengths = objects >>= mapM_ (\(num, obj) -> 
                                    assertEqual (errMsg obj num) 1 num)

