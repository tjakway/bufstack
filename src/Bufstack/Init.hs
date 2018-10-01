module Bufstack.Init where

-- contains entry point material


import Neovim

import Bufstack.Core
import qualified Bufstack.Vim.Exports

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = do
        env <- initBufstack
        wrapPlugin Plugin
            { environment = env,
              exports = Bufstack.Autocmds.exports
            }
