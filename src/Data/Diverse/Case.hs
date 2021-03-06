{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Case where

import Data.Diverse.TypeLevel
import Data.Kind

-- | This class allows defining handlers that can handle the 'Head' type in the @xs@ typelist.
-- In conjunction with 'Data.Diverse.Reiterate.Reiterate', you can define handlers that can handle all
-- the types in the @xs@ typelist.
--
-- See "Data.Diverse.CaseFunc" and "Data.Diverse.Cases".
class Case c (xs :: [Type]) where
    -- | Return the handler/continuation when x is observed.
    case' :: c xs -> Head xs -> CaseResult c (Head xs)
