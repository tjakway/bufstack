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

removeBuffer :: Nvim.Buffer -> BufstackM ()
removeBuffer b = modifyBuffersM_ $ filter (/= b)

-- | look at the top buffer without removing any
peekBuffer :: BufstackM (Maybe Nvim.Buffer)
peekBuffer = getBuffers >>= return . headMaybe
    where headMaybe [] = Nothing
          headMaybe (x:_) = Just x

replaceBufstack :: [Nvim.Buffer] -> BufstackM ()
replaceBufstack newBuffers = modifyBuffersM_ (const newBuffers)

reverseBufstack :: BufstackM ()
reverseBufstack = modifyBuffersM_ reverse
