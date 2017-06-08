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
import qualified GHC.Generics as G

-- | No Wrapped instance to protect the constructors to ensure that
-- a Catalog only contains a tuple of unique types.
-- Use '_Cataloged' iso to create a Catalog.
-- Example: @review _Cataloged' ("foo", 6)@
data family Catalog (xs :: [Type])

newtype instance Catalog '[] = Catalog0 ()
    deriving (Read, Show, Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a] = Catalog1 a
    deriving (Read, Show, Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b] = Catalog2 (a, b)
    deriving (Show, Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b, c] = C3 (a, b, c)
    deriving (Show, Eq, Ord, Ix, Bounded, G.Generic)

deriving instance (Distinct '[a, b], Read a, Read b) => Read (Catalog '[a, b])
deriving instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Catalog '[a, b, c])

-- | Safe constructor and destructor of Catalogs
-- which ensures the types are distinct.
instance Catalog '[] ~ t => Rewrapped (Catalog '[]) t
instance Wrapped (Catalog '[]) where
    type Unwrapped (Catalog '[]) = ()
    _Wrapped' = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE _Wrapped' #-}

instance Catalog '[a] ~ t => Rewrapped (Catalog '[a]) t
instance Wrapped (Catalog '[a]) where
    type Unwrapped (Catalog '[a]) = a
    _Wrapped' = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE _Wrapped' #-}

instance (Distinct '[a, b], Catalog '[a, b] ~ t) => Rewrapped (Catalog '[a, b]) t
instance Distinct '[a, b] => Wrapped (Catalog '[a, b]) where
    type Unwrapped (Catalog '[a, b]) = (a, b)
    _Wrapped' = iso (\(Catalog2 t) -> t) Catalog2
    {-# INLINE _Wrapped' #-}

catalog :: Wrapped (Catalog s) => Unwrapped (Catalog s) -> Catalog s
catalog = review _Cataloged'

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
    item = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE item #-}
instance Has a (Catalog '[a]) where
    item = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE item #-}
instance Distinct '[a, b] => Has a (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _1
    {-# INLINE item #-}
instance Distinct '[a, b] => Has b (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _2
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

instance Project (Catalog '[]) t where
    project f t = fmap (const t) (f $ Catalog0 ())
    {-# INLINE project #-}
instance Has a t => Project (Catalog '[a]) t where
    project f t = fmap (\(Catalog1 a) -> t & item .~ a) (f $ Catalog1 (t ^. item))
    {-# INLINE project #-}
instance (Distinct '[a, b], Has a t, Has b t) => Project (Catalog '[a, b]) t where
    project f t = fmap (\(Catalog2 (a, b)) -> t & item .~ a & item .~ b) (f $ Catalog2 (t ^. item, t ^. item))
    {-# INLINE project #-}
