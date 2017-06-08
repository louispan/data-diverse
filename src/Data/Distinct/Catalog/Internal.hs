{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.Catalog.Internal where

import Control.Lens
import Data.Distinct.TypeLevel
import Data.Ix
import Data.Kind

-- | No Wrapped instance to protect the constructors to ensure that
-- a Catalog only contains a tuple of unique types.
-- Use '_Cataloged' iso to create a Catalog.
-- Example: @review _Cataloged' ("foo", 6)@
data family Catalog (xs :: [Type])

-- FIXME: Make the constructor in Read/Show "Catalog"?

newtype instance Catalog '[] = C0 ()
    deriving (Show, Read, Eq, Ord, Ix, Bounded)
newtype instance Catalog '[a] = C1 a
    deriving (Show, Read, Eq, Ord, Ix, Bounded)
-- Use GADTs to bring in 'Distinct' constraints.
data instance Catalog '[a, b] where
    C2 :: Distinct '[a, b] => (a, b) -> Catalog '[a, b]
data instance Catalog '[a, b, c] where
    C3 :: Distinct '[a, b, c] => (a, b, c) -> Catalog '[a, b, c]

deriving instance (Distinct '[a, b], Read a, Read b) => Read (Catalog '[a, b])
deriving instance (Show a, Show b) => Show (Catalog '[a, b])
deriving instance (Eq a, Eq b) => Eq (Catalog '[a, b])
deriving instance (Ord a, Ord b) => Ord (Catalog '[a, b])
deriving instance (Ix a, Ix b) => Ix (Catalog '[a, b])
deriving instance (Distinct '[a, b], Bounded a, Bounded b) => Bounded (Catalog '[a, b])

deriving instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Catalog '[a, b, c])
deriving instance (Show a, Show b, Show c) => Show (Catalog '[a, b, c])
deriving instance (Eq a, Eq b, Eq c) => Eq (Catalog '[a, b, c])
deriving instance (Ord a, Ord b, Ord c) => Ord (Catalog '[a, b, c])
deriving instance (Ix a, Ix b, Ix c) => Ix (Catalog '[a, b, c])
deriving instance (Distinct '[a, b, c], Bounded a, Bounded b, Bounded c) => Bounded (Catalog '[a, b, c])

-- | Safe constructor and destructor of Catalogs
-- which ensures the types are distinct.
instance Catalog '[] ~ t => Rewrapped (Catalog '[]) t
instance Wrapped (Catalog '[]) where
    type Unwrapped (Catalog '[]) = ()
    _Wrapped' = iso (\(C0 x) -> x) C0
    {-# INLINE _Wrapped' #-}

instance Catalog '[a] ~ t => Rewrapped (Catalog '[a]) t
instance Wrapped (Catalog '[a]) where
    type Unwrapped (Catalog '[a]) = a
    _Wrapped' = iso (\(C1 x) -> x) C1
    {-# INLINE _Wrapped' #-}

instance (Distinct '[a, b], Catalog '[a, b] ~ t) => Rewrapped (Catalog '[a, b]) t
instance Distinct '[a, b] => Wrapped (Catalog '[a, b]) where
    type Unwrapped (Catalog '[a, b]) = (a, b)
    _Wrapped' = iso (\(C2 x) -> x) C2
    {-# INLINE _Wrapped' #-}

-- | Convenient version of '_Wrapped'' just for Catalog.
-- This can be used to construct Catalogs
-- Example: @review _Cataloged' ("foo", False, 5)
_Cataloged' :: Wrapped (Catalog s) => Iso' (Catalog s) (Unwrapped (Catalog s))
_Cataloged' = _Wrapped'

-- | Convenient version of '_Unwrapped'' just for Catalog.
_Uncataloged' :: Wrapped (Catalog s) => Iso' (Unwrapped (Catalog s)) (Catalog s)
_Uncataloged' = _Unwrapped'

-- | Convenient version of '_Wrapped' just for Catalog.
-- Use this to work under the Catalog wrapper.
_Cataloged :: Rewrapping (Catalog s) (Catalog t) => Iso (Catalog s) (Catalog t) (Unwrapped (Catalog s)) (Unwrapped (Catalog t))
_Cataloged = _Wrapped

-- | Convenient version of '_Unwrapped' just for Catalog.
_Uncataloged :: Rewrapping (Catalog s) (Catalog t) => Iso (Unwrapped (Catalog s)) (Unwrapped (Catalog t)) (Catalog s) (Catalog t)
_Uncataloged = _Unwrapped

-- | A catalog has a lens to an item.
class Has value record where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @item \@Int@
    item :: Lens' record value

instance Has () (Catalog '[]) where
    item = iso (\(C0 x) -> x) C0
    {-# INLINE item #-}
instance Has a (Catalog '[a]) where
    item = iso (\(C1 x) -> x) C1
    {-# INLINE item #-}
instance Distinct '[a, b] => Has a (Catalog '[a, b]) where
    item = iso (\(C2 x) -> x) C2 . _1
    {-# INLINE item #-}
instance Distinct '[a, b] => Has b (Catalog '[a, b]) where
    item = iso (\(C2 x) -> x) C2 . _2
    {-# INLINE item #-}

-------------------------------------------------
-- | Projection. A catalog can be narrowed or have its order changed by projecting
-- into another catalog type.
-- Basically the same class as 'Has' to prevent overlapping instances
class Project to from where
    -- | Narrow number of or change order of fields in a record.
    -- Use TypeApplication to specify the destination type.
    -- Example: @project \@(Catalog '[Int, String])@
    project :: Lens' from to

instance Project (Catalog '[]) r where
    project f s = fmap (const s) (f $ C0 ())
    {-# INLINE project #-}
instance Has a r => Project (Catalog '[a]) r where
    project f s = fmap (\(C1 a) -> s & item .~ a) (f $ C1 (s ^. item))
    {-# INLINE project #-}
instance (Distinct '[a, b], Has a r, Has b r) => Project (Catalog '[a, b]) r where
    project f s = fmap (\(C2 (a, b)) -> s & item .~ a & item .~ b) (f $ C2 (s ^. item, s ^. item))
    {-# INLINE project #-}
