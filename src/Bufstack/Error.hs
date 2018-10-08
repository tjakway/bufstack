module Bufstack.Error where

import Neovim
import Control.Exception (throw)
import System.IO

import Bufstack.Core
import Bufstack.Config.Type

printStderrOnError :: NeovimException -> IO ()
printStderrOnError = hPutStrLn stderr . show . pretty . show 

wrapErrors :: [NeovimException] -> NeovimException
wrapErrors exs = ErrorMessage $ (pretty "Multiple errors: ") <> (pretty . show $ exs)

handleError :: NeovimException -> BufstackM ()
handleError e = do
        onErr <- onErrorFIO . config <$> ask
        liftIO $ onErr e

handleErrorE :: Either NeovimException a -> BufstackM ()
handleErrorE (Right _) = return ()
handleErrorE (Left e) = handleError e

handleErrorME :: BufstackM (Either NeovimException a) -> BufstackM ()
handleErrorME e = e >>= handleErrorE

handleErrorWithDefault :: a -> BufstackM (Either NeovimException a) -> BufstackM a
handleErrorWithDefault defaultValue m = m >>= f
        where f (Right x) = return x
              f (Left y) = handleError y >> return defaultValue

printAndThrow :: NeovimException -> IO ()
printAndThrow e =  printStderrOnError e >> throw e


defaultOnError :: NeovimException -> IO ()
defaultOnError = printAndThrow
