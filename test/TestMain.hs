{-# Language ScopedTypeVariables #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

import Neovim.Test

import Bufstack.Core
import Bufstack.Test.Vim.Framework
import qualified Bufstack.Test.BasicTests as BasicTests

maxSecondsPerTest :: Seconds
maxSecondsPerTest = Seconds 60

testConfig :: Config
testConfig = defaultConfig

main :: IO ()
main =     -- create a new context for each test
       let testFunctionWithCleanup f = \input -> f input >> cleanupM
           wrapTest testF = initBufstackIO testConfig >>= (\env -> 
                                testWithEmbeddedNeovim 
                                    Nothing 
                                    maxSecondsPerTest 
                                    env
                                    (setup >>= testFunctionWithCleanup testF) )

           -- tests form a tree
           groupTests tests name = testGroup name $ map (\(t, n) -> testCase n (wrapTest t)) tests
           allTests = map (uncurry groupTests) [(BasicTests.tests, "BasicTests")]

        in defaultMainWithOpts 
            allTests
            mempty
