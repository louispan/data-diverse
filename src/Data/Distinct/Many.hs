{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Distinct.Many where

import Data.Distinct.TypeLevel
import qualified GHC.Generics as G

-- | A polymorphic variant or co-record where there are no duplicates in the type list of possible types.
-- This means TypeApplication (instead of labels) can be used to index the variant.
-- This is essentially a typed version of 'Data.Dynamic'
--
-- This encoding uses a data family to allow different sets of GADT constructors,
-- with different number of constuctors in each set.
-- GADTs are used to ensure the 'Distinct' constraint is met.
data family Many (xs :: [*])

data instance Many '[] = M0
    deriving (Eq, Show, Ord, G.Generic)
newtype instance Many '[a] = M1 a
    deriving (Eq, Show, Ord, G.Generic)
data instance Many '[a, b] where
    M2_1 :: Distinct '[a, b] => a -> Many '[a, b]
    M2_2 :: Distinct '[a, b] => b -> Many '[a, b]
data instance Many '[a, b, c] where
    M3_1 :: Distinct '[a, b, c] => a -> Many '[a, b, c]
    M3_2 :: Distinct '[a, b, c] => b -> Many '[a, b, c]
    M3_3 :: Distinct '[a, b, c] => c -> Many '[a, b, c]

deriving instance (Eq a, Eq b) => Eq (Many '[a, b])
deriving instance (Ord a, Ord b) => Ord (Many '[a, b])
deriving instance (Show a, Show b) => Show (Many '[a, b])
deriving instance (Distinct '[a, b], Read a, Read b) => Read (Many '[a, b])
deriving instance (Eq a, Eq b, Eq c) => Eq (Many '[a, b, c])
deriving instance (Ord a, Ord b, Ord c) => Ord (Many '[a, b, c])
deriving instance (Show a, Show b, Show c) => Show (Many '[a, b, c])
deriving instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Many '[a, b, c])

-- -- | Get the index of the variant
-- index :: Many a -> Word
-- index (Many n _) = n

-- TODO:

-- Naming: reinterpret_cast, dynamic_cast ?


-- To have prism from Many to inner type
-- This also allows safe construction.

-- To project from larger Many to smaller Many

-- Show and Read instances

-- disallow empty many
