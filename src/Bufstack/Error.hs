module Bufstack.Error where

import Neovim
import Control.Exception (throw)
import System.IO

import Bufstack.Core
import Bufstack.Config.Type

throwOnError :: NeovimException -> Neovim env ()
throwOnError = return . throw . pretty

printStderrOnError :: NeovimException -> IO ()
printStderrOnError = hPutStrLn stderr . show . pretty 

onErrorM :: NeovimException -> BufstackM ()
onErrorM e = do
        onErr <- onErrorFIO . config <$> ask
        liftIO $ onErr e

        

wrapErrors :: [NeovimException] -> NeovimException
wrapErrors exs = ErrorMessage $ (pretty "Multiple errors: ") <> (pretty exs)

handleError :: NeovimException -> BufstackM ()
handleError = liftIO . onErrorFIO . config <$> ask

handleErrorE :: Either NeovimException a -> BufstackM ()
handleErrorE (Right _) = return ()
handleErrorE (Left e) = handleError e

handleErrorME :: BufstackM (Either NeovimException a) -> BufstackM ()
handleErrorME e = e >>= handleErrorE

printAndThrow :: NeovimException -> BufstackM ()
printAndThrow e =  printStderrOnError' e >> throwN e
    where throwN = liftIO throw
          printStderrOnError' = liftIO printStderrOnError



defaultOnError :: NeovimException -> BufstackM ()
defaultOnError = printAndThrow
