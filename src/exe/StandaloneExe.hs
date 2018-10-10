import System.IO

import Neovim
import qualified Bufstack.Vim.Exports as Bufstack


main :: IO ()
main = neovim defaultConfig
        { plugins = [ Bufstack.plugin ] ++ plugins defaultConfig
        }
