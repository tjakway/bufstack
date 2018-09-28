module Bufstack.Util where

import qualified Control.Concurrent.STM as STM
import qualified Neovim as Nvim
import qualified Neovim.API.String as Nvim
import Control.Monad (filterM)

atomically :: STM.STM a -> Nvim.Neovim env a
atomically = Nvim.liftIO . STM.atomically
