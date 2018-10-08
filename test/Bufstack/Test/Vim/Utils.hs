module Bufstack.Test.Vim.Utils where

import qualified Test.HUnit as HUnit

import Neovim

-- lifted hunit assertions

assertFail :: String -> Neovim env ()
assertFail = liftIO . Hunit.assertFail

assertEqual :: (Eq a, Show a) => String -> a -> a -> Neovim env ()
assertEqual a b = liftIO . HUnit.assertEqual a b

assertTrue :: String -> Bool -> Neovim env ()
assertTrue a = liftIO . HUnit.assertBool a

assertFailure :: String -> Neovim env ()
assertFailure = liftIO . HUnit.assertFailure

assertCurrentBufEquals :: Buffer -> Neovim env ()
assertCurrentBufEquals expected = do
        currentBuf <- vim_get_current_buffer'
        assertEqual "Assert current buf equals" expected currentBuf

-- | check that the top of the bufstack is equal to expected
assertPeekBufstack :: Buffer -> BufstackM ()
assertPeekBufstack expected = do
        let msg = "Assert top of bufstack == " ++ show expected
        top <- peekBuffer
        case top of Just x -> assertEqual msg expected x
                    Nothing -> assertFail msg

-- | check that the underlying bufstack is equal to expected
assertBufstackEquals :: [Buffer] -> BufstackM ()
assertBufstackEquals expected = 
        let msg = "Check bufstack is equal to expected"
            in getBuffers >>= assertEqual msg expected


