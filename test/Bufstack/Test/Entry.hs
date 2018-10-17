{-# Language ScopedTypeVariables #-}
module Bufstack.Test.Entry where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

import System.IO
import Control.Exception (throwIO)
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
testConfig = let onError ex = do
                    hPutStrLn stderr (show ex)
                    throwIO ex
                in defaultConfig {onErrorFIO = onError}

-- | main function
entry :: IO ()
entry =     -- create a new context for each test
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
