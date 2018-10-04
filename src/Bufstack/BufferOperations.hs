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

nextBufFunction :: BufstackM ()
nextBufFunction = undefined  -- TODO

-- | TODO: use the previous buffer (numerically) if the stack is empty
-- (i.e. if popBuffer returns Nothing)
previousBufFunction :: BufstackM ()
previousBufFunction = popBuffer >>= f
    where f (Just buf) = Nvim.nvim_set_current_buf' buf
          f _ = return ()
