{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Case where

import Data.Diverse.Type
import Data.Kind
import GHC.Prim (Any)
import Unsafe.Coerce

-- | This class allows defining handlers that can handle the 'Head' type in the @xs@ typelist.
-- In conjunction with 'Data.Diverse.Reiterate.Reiterate', you can define handlers that can handle all
-- the types in the @xs@ typelist.
--
-- See "Data.Diverse.CaseTypeable" and "Data.Diverse.Cases".
class Case c (xs :: [Type]) r where
    -- | Return the handler/continuation when x is observed.
    case' :: c xs r -> Head xs -> r
    case' c v = caseAny c (unsafeCoerce v)

    -- | A variation of 'case'' where x is left as 'Any'
    caseAny :: c xs r -> Any -> r
    caseAny c v = case' c (unsafeCoerce v)
