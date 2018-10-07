module Bufstack.Test.Vim.Utils where

import qualified Test.HUnit as HUnit

import Neovim

-- lifted hunit assertions

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
