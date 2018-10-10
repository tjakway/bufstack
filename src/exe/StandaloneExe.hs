module Main where

import Neovim
import qualified Bufstack.Vim.Exports as Bufstack

import System.IO
import System.IO.Error
import System.Directory
import System.FilePath
import System.Exit

main :: IO ()
main = do
        let logFile = "bufstack.log"
            logLevel = DEBUG
            handleRemoveFileException f e
                -- continue if the file doesn't exist
                | isDoesNotExistError e = return ()
                -- print & fail for other errors
                | otherwise = do
                    hPutStrLn stderr $ "Error while removing log file " ++
                        show f ++ ": " ++ show e
                    exitWith (ExitFailure 1)


        logPath <- (</> logFile) <$> getCurrentDirectory

        removeFile logPath `catchIOError` handleRemoveFileException logPath

        neovim defaultConfig
            { plugins = [ Bufstack.plugin ] ++ plugins defaultConfig,
              logOptions = Just (logFile, logLevel)
            }
