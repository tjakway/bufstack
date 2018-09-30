module Bufstack.Class.IsValid where

import Neovim
import Neovim.API.String

class IsValid a where
        isValid :: a -> Neovim env Bool
        isValid' :: a -> Neovim env (Either NeovimException Bool)

instance IsValid Buffer where
        isValid = nvim_buf_is_valid
        isValid' = nvim_buf_is_valid'

instance IsValid Window where
        isValid = window_is_valid
        isValid' = window_is_valid'

instance IsValid Tabpage where
        isValid = tabpage_is_valid
        isValid' = tabpage_is_valid'
