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
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.Catalog.Internal where

import Control.Lens
import Data.Distinct.TypeLevel
import Data.Ix
import Data.Kind
import qualified GHC.Generics as G
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L

-- | A Catalog is an anonymous product type (also know as polymorphic record), that has fields of distinct types.
-- That is, there are no duplicates types in the fields of the record.
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to get and set fields in the Catalog.
data family Catalog (xs :: [Type])

newtype instance Catalog '[] = Catalog0 ()
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a] = Catalog1 a
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b] = Catalog2 (a, b)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
newtype instance Catalog '[a, b, c] = Catalog3 (a, b, c)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)

-- deriving instance (Distinct '[a, b], Read a, Read b) => Read (Catalog '[a, b])
-- deriving instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Catalog '[a, b, c])

readCatalog :: Read t => (t -> a) -> ReadPrec a
readCatalog f = parens $ prec 10 $ do
    lift $ L.expect (Ident "Catalog")
    t <- step readPrec
    pure (f t)

showCatalog :: Show a => Int -> a -> ShowS
showCatalog d t = showParen (d >= 11) ((showString "Catalog ") . (showsPrec 11 t))

instance (Distinct '[], AllRead '[]) => Read (Catalog '[]) where
    readPrec = readCatalog Catalog0

instance (Distinct '[a], AllRead '[a]) => Read (Catalog '[a]) where
    readPrec = readCatalog Catalog1

instance (Distinct '[a, b], AllRead '[a, b]) => Read (Catalog '[a, b]) where
    readPrec = readCatalog Catalog2

instance (Distinct '[a, b, c], AllRead [a, b, c]) => Read (Catalog '[a, b, c]) where
    readPrec = readCatalog Catalog3

instance (AllShow '[]) => Show (Catalog '[]) where
    showsPrec d (Catalog0 t) = showCatalog d t

instance (AllShow '[a]) => Show (Catalog '[a]) where
    showsPrec d (Catalog1 t) = showCatalog d t

instance (AllShow '[a, b]) => Show (Catalog '[a, b]) where
    showsPrec d (Catalog2 t) = showCatalog d t

instance (AllShow '[a, b, c]) => Show (Catalog '[a, b, c]) where
    showsPrec d (Catalog3 t) = showCatalog d t

------------------------------------------------------

-- | Convenient function to create Catalogs from tuples.
-- The @xs ~ TypesOf (TupleOf xs)@ helps type inference
catalog :: (Cataloged xs, xs ~ TypesOf (TupleOf xs)) => TupleOf xs -> Catalog xs
catalog = review _Cataloged
{-# INLINE catalog #-}

-- | Retrieves the tuple from a Catalog
-- The @xs ~ TypesOf (TupleOf xs)@ helps type inference
toTuple :: (Cataloged xs, xs ~ TypesOf (TupleOf xs)) => Catalog xs -> TupleOf xs
toTuple = view _Cataloged
{-# INLINE toTuple #-}

class Cataloged (xs :: [Type]) where
    _Cataloged :: Iso' (Catalog xs) (TupleOf xs)

-- | Safe constructor and destructor of Catalogs which ensures the types are distinct.
instance Cataloged '[] where
    _Cataloged = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE _Cataloged #-}
instance Cataloged '[a] where
    _Cataloged = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE _Cataloged #-}
instance (Distinct '[a, b]) => Cataloged '[a, b] where
    _Cataloged = iso (\(Catalog2 t) -> t) Catalog2
    {-# INLINE _Cataloged #-}

------------------------------------------------------
-- | Everything in the given typelist must be a Item instance
type family Items (s :: Type) (xs :: [Type]) :: Constraint where
   Items s '[] = ()
   Items s (x ': xs) = (Item x s, Items s xs)

-- | A catalog has a lens to an item.
class Item value record where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @item \@Int@
    item :: Lens' record value

-- | Get a value from a catalog. Not called @get@ to be consistent with 'replace'.
fetch :: Item value record => record -> value
fetch = view item
{-# INLINE fetch #-}

-- | Set a value in a Catalog. Not called @set@ as it conflicts with 'Control.Lens.set'
replace :: Item value record => value -> record -> record
replace = set item
{-# INLINE replace #-}

instance Item () (Catalog '[]) where
    item = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE item #-}
instance Item a (Catalog '[a]) where
    item = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE item #-}
instance Item a (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _2
    {-# INLINE item #-}

------------------------------------------------------

-- | Projection.
-- Basically the same class as 'Item' to prevent overlapping instances
-- A Catalog can be narrowed or have its order changed by projecting into another Catalog type.
-- Use TypeApplication to specify the destination type of the lens.
class Project to from where
    -- | Narrow number of or change order of fields in a record.
    -- Use TypeApplication to specify the destination type.
    -- Example: @project \@(Catalog '[Int, String])@
    project :: Lens' from to

narrow :: Project to from => from -> to
narrow = view project

amend :: Project to from => to -> from -> from
amend = set project

instance Project (Catalog '[]) t where
    project f t = fmap (const t) (f $ Catalog0 ())
    {-# INLINE project #-}
instance (Distinct '[a], Items t '[a]) => Project (Catalog '[a]) t where
    project f t = fmap (\(Catalog1 a) -> t & item .~ a) (f $ Catalog1 (t ^. item))
    {-# INLINE project #-}
-- Items results in UndecidableInstance. Safe because it just expands constraints.
instance (Distinct '[a, b], Items t '[a, b]) => Project (Catalog '[a, b]) t where
    project f t = fmap (\(Catalog2 (a, b)) -> t & item .~ a & item .~ b) (f $ Catalog2 (t ^. item, t ^. item))
    {-# INLINE project #-}

------------------------------------------------------

-- FIXME: Read/Show with contructor as just "Catalog", don't worry about Catalog1 - let it be inferred
