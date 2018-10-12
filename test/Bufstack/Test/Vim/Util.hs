module Bufstack.Test.Vim.Util (
  assertEqual,
  assertTrue,
  assertFailure,
  assertCurrentBufEquals,
  assertPeekBufstack,
  assertBufstackEquals,
  numBuffers,
  mkBuffers
) where

import qualified Test.HUnit as HUnit
import GHC.Stack

import Control.Monad (when)
import System.IO

import Neovim

import Bufstack.Core
import Bufstack.BufferOperations

-- lifted hunit assertions

printStack :: HasCallStack => Bool -> Neovim env ()
printStack cond = liftIO $ when cond print
    where print = hPutStrLn stderr . prettyCallStack $ callStack

assertEqual :: (HasCallStack, Eq a, Show a) => String -> a -> a -> Neovim env ()
assertEqual msg a b = printStack (a /= b) >> (liftIO $ HUnit.assertEqual msg a b)

assertTrue :: HasCallStack => String -> Bool -> Neovim env ()
assertTrue msg a = printStack (not a) >> (liftIO $ HUnit.assertBool msg a)

assertFailure :: HasCallStack => String -> Neovim env ()
assertFailure msg = printStack True >> (liftIO . HUnit.assertFailure $ msg)

assertCurrentBufEquals :: HasCallStack => Buffer -> Neovim env ()
assertCurrentBufEquals expected = do
        currentBuf <- vim_get_current_buffer'
        assertEqual "Assert current buf equals" expected currentBuf

-- | check that the top of the bufstack is equal to expected
assertPeekBufstack :: HasCallStack => Buffer -> BufstackM ()
assertPeekBufstack expected = do
        let msg = "Assert top of bufstack == " ++ show expected
        top <- peekBuffer
        case top of Just x -> assertEqual msg expected x
                    Nothing -> assertFailure msg

-- | check that the underlying bufstack is equal to expected
assertBufstackEquals :: HasCallStack => [Buffer] -> BufstackM ()
assertBufstackEquals expected = 
        let msg = "Check bufstack is equal to expected"
            in getBuffers >>= assertEqual msg expected

numBuffers :: Neovim env Int
numBuffers = length <$> vim_get_buffers'

mkBuffers :: Int -> Neovim env ()
mkBuffers howMany = do
        startingBuf <- vim_get_current_buffer'
        numBuffers' <- numBuffers

        -- see https://forum.upcase.com/t/vimrc-winminwidth-e36-not-enough-room-error/4334
        -- regarding silencing out-of-width errors (which we don't care
        -- about because we're headless)
        mapM_ (const $ vim_command' "silent! new") [1..howMany]
        nvim_set_current_buf' startingBuf

        let msg = "Check we created the correct number of buffers"
            expectedNumBuffers = numBuffers' + howMany

        numBuffers >>= assertEqual msg expectedNumBuffers