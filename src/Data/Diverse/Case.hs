{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Case where

import Data.Diverse.Type
import Data.Kind

-- | This class allows storing polymorphic functions with extra constraints that is used on each iteration of 'Switch'.
-- An instance of this knows how to construct a handler for the first type in the 'xs' typelist, or
-- how to construct the remaining 'Case's for the rest of the types in the type list.
class Case c (xs :: [Type]) r where
    -- | Return the handler/continuation when x is observed.
    case' :: c xs r -> (Head xs -> r)
