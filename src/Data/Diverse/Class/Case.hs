{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Class.Case where

import Data.Diverse.Class.Reiterate
import Data.Diverse.Type
import Data.Kind

-- | This class allows storing polymorphic functions with extra constraints that is used on each iteration of 'Switch'.
-- An instance of this knows how to construct a handler for the first type in the 'xs' typelist, or
-- how to construct the remaining 'Case's for the rest of the types in the type list.
class Reiterate c xs => Case c (xs :: [Type]) r where
    -- | Return the handler/continuation when x is observed.
    then' :: c xs r -> (Head xs -> r)
