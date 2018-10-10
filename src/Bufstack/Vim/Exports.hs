{-# LANGUAGE TemplateHaskell #-}
module Bufstack.Vim.Exports (
    plugin                            
) where

import Neovim

import qualified Neovim.Plugin.Internal as Nvim

import qualified Bufstack.Core as Bufstack
import qualified Bufstack.Config.Default as Bufstack
import Bufstack.Util
import Bufstack.BufferOperations
import Bufstack.Vim.Autocmds

exportedFunctions :: [Nvim.ExportedFunctionality Bufstack.Bufstack]
exportedFunctions = [$(function "BufstackNextBufFunction" 'nextBufFunction) Sync,
          $(function "BufstackPrevBufFunction" 'prevBufFunction) Sync,
          $(function "BufstackPrintBufstack" 'printBufstack) Sync]

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
        env <- atomically $ Bufstack.initBufstack Bufstack.defaultConfig 
        wrapPlugin (Plugin
            { environment = env
            , exports     = exportedFunctions
            })
