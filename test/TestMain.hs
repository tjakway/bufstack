{-# Language ScopedTypeVariables #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

import qualified Neovim

import Bufstack.Core
import Bufstack.Init
import Bufstack.Config.Type
import Bufstack.Config.Default
import Bufstack.Test.Vim.Framework
import qualified Bufstack.Test.BasicTests as BasicTests
import qualified Bufstack.Test.Vim.EmbeddedTest as EmbeddedTest

maxSecondsPerTest :: EmbeddedTest.Seconds
maxSecondsPerTest = EmbeddedTest.Seconds 60

testConfig :: Config
testConfig = defaultConfig

main :: IO ()
main =     -- create a new context for each test
       let testFunctionWithCleanup env bufstackSetupFunction handles f = do
                        bufstackSetupFunction env
                        input <- genericTestSetup
                        f input
                        cleanupM
                        exitNeovim handles

           wrapTest testF = initBufstackIO testConfig >>= (\(env,setupFunction) -> 
                                EmbeddedTest.testWithEmbeddedNeovim 
                                    Nothing 
                                    maxSecondsPerTest 
                                    env 
                                    (\handles ->
                                        testFunctionWithCleanup env setupFunction handles testF) )

           -- tests form a tree
           groupTests tests name = testGroup name $ map (\(t, n) -> testCase n (wrapTest t)) tests
           allTests = map (uncurry groupTests) [(BasicTests.tests, "BasicTests")]

        in defaultMainWithOpts 
            allTests
            mempty
