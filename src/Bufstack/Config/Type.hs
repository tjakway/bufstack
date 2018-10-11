module Bufstack.Config.Type where

import Neovim

data Config = 
    Config {
        --to indicate their position on the stack
        --TODO: not implemented
        renameBuffers :: Bool,
        removeDuplicates :: Bool,
        onErrorFIO :: NeovimException -> IO ()
    }
