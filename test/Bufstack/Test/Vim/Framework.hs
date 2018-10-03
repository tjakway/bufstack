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
import qualified Bufstack.Class.Closeable as Closeable

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

setup :: BufstackM ()
setup = do
        (thisTabpage, thisBuffer) <- newTabpage
        thisWindow <- tabpage_get_window' thisTabpage

        allTabpages <- vim_get_tabpages'
        allWindows <- vim_get_windows'
        allBuffers <- vim_get_buffers'

        -- close other windows and tabpages
        closeAll' . filter (/= thisTabpage) $ allTabpages
        closeAll' . filter (/= thisWindow) $ allWindows
        closeAll' . filter (/= thisBuffer) $ allBuffers

        vim_get_tabpages' >>= assertEqual "Closed other tabpages" [thisTabpage]
        vim_get_windows' >>= assertEqual "Closed other windows" [thisWindow]
        vim_get_buffers' >>= assertEqual "Closed other buffers" [thisBuffer]

    where newTabpage :: BufstackM (Tabpage, Buffer)
          newTabpage = do
              vim_command' ":tabnew"
              newTp <- vim_get_current_tabpage'
              newBuffer <- vim_get_current_buffer'
              tabpage_is_valid' newTp >>= assertTrue "new tab page is valid"
              buffer_is_valid' newBuffer >>= assertTrue "new buffer is valid"
              return (newTp, newBuffer)

          closeAll' :: Closeable.Closeable a => [a] -> Neovim env ()
          closeAll' = Closeable.closeAll' (\e -> 
                            assertFailure $ "closeAll' failed: " ++ show e)
