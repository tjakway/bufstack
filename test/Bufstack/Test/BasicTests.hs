{-# LANGUAGE ScopedTypeVariables #-}
module Bufstack.Test.BasicTests (tests) where

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit

import Data.List
import Data.Function (on)
import Control.Monad

import Neovim
import Neovim.API.String

import Bufstack.Core
import Bufstack.Util
import Bufstack.BufferOperations
import Bufstack.Test.Vim.Framework

import Bufstack.Test.Vim.Framework
import Bufstack.Test.Vim.Util

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
        prevBufFunction

        currentBuf <- vim_get_current_buffer'
        assertTrue "Buffer should have changed" (openBuf /= currentBuf)
        assertEqual "Should have switched back to the previous buffer" buf currentBuf

doTestWraparound :: BufstackM ()
doTestWraparound = do
        startingBuf <- vim_get_current_buffer'
        startingBufstack <- getBuffers
        bufs <- getNumberedBuffers'
        
        let cmp f = f (compare `on` snd) bufs
            highestBuf = fst $ cmp maximumBy 
            lowestBuf = fst $ cmp minimumBy

        -- check that we wrap around from the highest buf to the lowest buf
        nvim_set_current_buf' highestBuf
        nextBufFunction
        assertCurrentBufEquals lowestBuf
        assertPeekBufstack highestBuf

        prevBufFunction 
        assertCurrentBufEquals highestBuf
        -- we should have popped 1 buffer off, leaving us where we started
        assertBufstackEquals startingBufstack


-- | test that if we're currently at the highest numbered buffer that we'll
-- wraparound to the lowest numbered buffer
bufTestWraparound :: BufstackTest
bufTestWraparound (_, _, _) = do
        mkBuffers 10
        doTestWraparound

bufNextTest :: BufstackTest
bufNextTest (_, _, buf) = do
        mkBuffers 10
        sequence_ . take 50 . repeat $ (nextBufFunction >> assertNoAdjacentDuplicates)

    where assertNoAdjacentDuplicates :: BufstackM ()
          assertNoAdjacentDuplicates = 
            let f (duplicates, Just prev) e = if e == prev then (e : duplicates, Just e)
                                                           else (duplicates, Just e)
                f (duplicates, Nothing) e = (duplicates, Just e)
                check = assertEqual "Expected no duplicate buffers" []
                in getBuffers >>= (check . reverse . fst . foldl' f ([], Nothing))



tests :: [BufstackTestCase]
tests = [(bufPrevTest, "bufPrevTest"), (bufTestWraparound, "bufTestWraparound"),
         (bufNextTest, "bufNextTest")]
