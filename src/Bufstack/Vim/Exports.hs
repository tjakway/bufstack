{-# LANGUAGE TemplateHaskell #-}
module Bufstack.Vim.Exports where

import Neovim

import qualified Neovim.Plugin.Internal as Nvim

import Bufstack.Core
import Bufstack.BufferOperations
import Bufstack.Vim.Autocmds

exports :: [Nvim.ExportedFunctionality Bufstack]
exports = [$(function' 'nextBufFunction) Async,
          $(function' 'prevBufFunction) Async,
          $(function' 'printBufstack) Async]
