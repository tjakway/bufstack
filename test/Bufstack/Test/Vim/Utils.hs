module Bufstack.Test.Vim.Utils where

import qualified Test.HUnit as HUnit

import Neovim

assertEqual :: (Eq a, Show a) => String -> a -> a -> Neovim env ()
assertEqual a b = liftIO . HUnit.assertEqual a b
