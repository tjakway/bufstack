{-# Language ExistentialQuantification #-}
module Bufstack.Class.IsValid where

import Neovim
import Neovim.API.String

import Bufstack.Util

class IsValid a where
        isValid :: a -> Neovim env (Either NeovimException Bool)
        isValid' :: a -> Neovim env Bool

instance IsValid Buffer where
        isValid = nvim_buf_is_valid
        isValid' = nvim_buf_is_valid'

instance IsValid Window where
        isValid = window_is_valid
        isValid' = window_is_valid'

instance IsValid Tabpage where
        isValid = tabpage_is_valid
        isValid' = tabpage_is_valid'

-- | existentially qualified instance wrapper
data IsValidInst = forall a. IsValid a => IsValidInst a

-- obviously
instance IsValid IsValidInst where
        isValid (IsValidInst a) = isValid a
        isValid' (IsValidInst a) = isValid' a


whenIsValid :: IsValid a => a -> Neovim env (Either NeovimException ()) -> Neovim env (Either NeovimException ())
whenIsValid a f = isValid a `bindNvimEither` (\x -> if x then f else return (Right ()))

-- | like whenIsValid when the first argument also needs to be unwrapped
whenIsValidM :: IsValid a => Neovim env (Either NeovimException a) -> 
                                Neovim env (Either NeovimException a) ->
                                Neovim env (Either NeovimException a)
whenIsValidM a = a `bindNvimEither` whenIsValid
