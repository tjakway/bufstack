module Bufstack.Class.HasNumber where

import Neovim
import Neovim.API.String

-- | this is the type returned by nvim-hs
type NvimNumber = Int64

class NvimHasNumber a where
        getNumber :: a -> Neovim env (Either NeovimException NvimNumber)
        getNumber' :: a -> Neovim env NvimNumber

instance NvimHasNumber Buffer where
        getNumber = buffer_get_number
        getNumber' = buffer_get_number'

instance NvimHasNumber Window where
        getNumber = nvim_win_get_number
        getNumber' = nvim_win_get_number'

instance NvimHasNumber Tabpage where
        getNumber = nvim_tabpage_get_number
        getNumber' = nvim_tabpage_get_number'
