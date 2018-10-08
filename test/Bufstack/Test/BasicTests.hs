module Bufstack.Test.BasicTests (tests) where

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit

import Data.List

import Neovim
import Neovim.API.String

import Bufstack.Core
import Bufstack.Util
import Bufstack.BufferOperations
import Bufstack.Test.Vim.Framework

import Bufstack.Test.Vim.Framework
import Bufstack.Test.Vim.Utils

bufPrevTest :: BufstackTest
bufPrevTest (_, _, buf) = do
        -- open a new buffer
        vim_command' "new"

        buffers <- nub <$> vim_get_buffers'
        assertEqual "Only 2 buffers" 2 (length buffers)

        let otherBuffer = head $ filter (/= buf) buffers

        openBuf <- vim_get_current_buffer'
        assertEqual "The other buffer should be open" otherBuffer openBuf

        -- replace the bufstack with the first buffer we had
        replaceBufstack [buf]
        previousBufFunction

        currentBuf <- vim_get_current_buffer'
        assertTrue "Buffer should have changed" (openBuf /= currentBuf)
        assertEqual "Should have switched back to the previous buffer" buf currentBuf


bufNextTest :: BufstackTest
bufNextTest (_, _, buf) = do
        secondBuf <- newBuffer'
        assertCurrentBufEquals secondBuf

        -- test wraparound to the first buffer




tests :: [BufstackTestCase]
tests = [(bufPrevTest, "bufPrevTest")]
