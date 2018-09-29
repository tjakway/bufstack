module Bufstack.BufferOperations where

import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Bufstack.Core
import Bufstack.Util

pushBuffer :: Nvim.Buffer -> BufstackM ()
pushBuffer buf = modifyBuffersM_ (buf :)

popBuffer :: BufstackM (Maybe Nvim.Buffer)
popBuffer = modifyBuffersMs tailMaybeS 
    where tailMaybeS [] = (Nothing, [])
          tailMaybeS (x:xs) = (Just x, xs)
