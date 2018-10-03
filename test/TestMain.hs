module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

import qualified Bufstack.Test.BasicTests as BasicTests

main :: IO ()
main = defaultMainWithOpts 
            [BasicTests.tests]
            mempty
