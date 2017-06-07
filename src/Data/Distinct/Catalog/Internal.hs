{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Distinct.Catalog.Internal where

import Control.Lens
import Data.Distinct.TypeLevel
import Data.Ix
import qualified GHC.Generics as G
import Text.Read

-- | No Wrapped instance to protect the constructors to ensure that
-- a Catalog only contains a tuple of unique types.
-- Use '_Cataloged' iso to create a Catalog.
-- Example: @review _Cataloged' ("foo", 6)@
data family Catalog (xs :: [*])

newtype instance Catalog '[] = C0 ()
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a] = C1 a
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b] = C2 (a, b)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b, c] = C3 (a, b, c)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)

instance Read (Catalog '[]) where
    readPrec = C0 <$> readPrec
instance Show (Catalog '[]) where
    show (C0 v) = show v
instance Read a => Read (Catalog '[a]) where
    readPrec = C1 <$> readPrec
instance Show a => Show (Catalog '[a]) where
    show (C1 v) = show v
instance (Distinct '[a, b], Read a, Read b) => Read (Catalog '[a, b]) where
    readPrec = C2 <$> readPrec
instance (Show a, Show b) => Show (Catalog '[a, b]) where
    show (C2 v) = show v

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
-- Use this to to construct catalogs from tuples.
-- Example: @review _Cataloged' ("foo", 6)@
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
instance Has a (Catalog '[a, b]) where
    item = iso (\(C2 x) -> x) C2 . _1
    {-# INLINE item #-}
instance Has b (Catalog '[a, b]) where
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
instance (Has a r, Has b r) => Project (Catalog '[a, b]) r where
    project f s = fmap (\(C2 (a, b)) -> s & item .~ a & item .~ b) (f $ C2 (s ^. item, s ^. item))
    {-# INLINE project #-}
