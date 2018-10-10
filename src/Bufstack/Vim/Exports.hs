{-# LANGUAGE TemplateHaskell #-}
module Bufstack.Vim.Exports where

import Neovim

import qualified Neovim.Plugin.Internal as Nvim

import qualified Bufstack.Core as Bufstack
import qualified Bufstack.Config.Default as Bufstack
import Bufstack.Util
import Bufstack.BufferOperations
import Bufstack.Vim.Autocmds

exportedFunctions :: [Nvim.ExportedFunctionality Bufstack.Bufstack]
exportedFunctions = [$(function' 'nextBufFunction) Async,
          $(function' 'prevBufFunction) Async,
          $(function' 'printBufstack) Async]

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
        env <- atomically $ Bufstack.initBufstack Bufstack.defaultConfig 
        wrapPlugin (Plugin
            { environment = env
            , exports     = exportedFunctions
            })
