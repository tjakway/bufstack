module Main where

import Neovim
import qualified Bufstack.Vim.Exports as Bufstack

import System.Directory
import System.FilePath

main :: IO ()
main = do
        let logFile = "bufstack.log"
            logLevel = DEBUG

        logPath <- (</> logFile) <$> getCurrentDirectory
        neovim defaultConfig
            { plugins = [ Bufstack.plugin ] ++ plugins defaultConfig,
              logOptions = Just (logFile, logLevel)
            }
