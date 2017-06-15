{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.Class.Reduce where

import Data.Kind

-- | Reduce a @t xs@ into @r@ using handlers.
class Reduce t handler (xs :: [Type]) r where
    reduce :: handler xs r -> t xs -> r
