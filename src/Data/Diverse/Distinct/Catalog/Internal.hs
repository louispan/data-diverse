{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Distinct.Catalog.Internal where

import Control.Lens
import Data.Diverse.Type
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

------------------------------------------------------

-- | Internal helper for 'Read' instance
readCatalog :: Read t => (t -> a) -> ReadPrec a
readCatalog f = parens $ prec 10 $ do
    lift $ L.expect (Ident "Catalog")
    t <- step readPrec
    pure (f t)
{-# INLINE readCatalog #-}

-- | Internal helper for 'Show' instance
showCatalog :: Show a => Int -> a -> ShowS
showCatalog d t = showParen (d >= 11) ((showString "Catalog ") . (showsPrec 11 t))
{-# INLINE showCatalog #-}

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

------------------------------------------------------
-- | Everything in the given typelist must be a Item instance
type family AllItem (s :: Type) (xs :: [Type]) :: Constraint where
   AllItem s '[] = ()
   AllItem s (x ': xs) = (Item x s, AllItem s xs)

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

------------------------------------------------------

-- | Projection.
-- A Catalog can be narrowed or have its order changed by projecting into another Catalog type.
-- Basically the same class as 'Item' to prevent overlapping instances
-- Use TypeApplication to specify the @to@ destination type of the lens.
-- Example: @project \@(Catalog '[Int, String])@
class Project to from where
    project :: Lens' from to

-- | @get@ a smaller Catalog from a larger Catalog
narrow :: Project to from => from -> to
narrow = view project
{-# INLINE narrow #-}

-- | @set@ the smaller Catalog in a larger Catalog
amend :: Project to from => to -> from -> from
amend = set project
{-# INLINE amend #-}

-- | This is 'project' with the type parameters reversed
-- so TypeApplications can be used to specify @from@ instead of @to@.
-- Example: @projected \@(Catalog '[Int, String])@
projected :: forall from to. Project to from => Lens' from to
projected = project
{-# INLINE projected #-}

------------------------------------------------------

newtype instance Catalog '[] = Catalog0 ()
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
instance Read (Catalog '[]) where
    readPrec = readCatalog Catalog0
    {-# INLINE readPrec #-}
instance Show (Catalog '[]) where
    showsPrec d (Catalog0 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance Cataloged '[] where
    _Cataloged = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE _Cataloged #-}
instance Project (Catalog '[]) t where
    project f t = fmap (const t) (f $ Catalog0 ())
    {-# INLINE project #-}
instance Item () (Catalog '[]) where
    item = iso (\(Catalog0 t) -> t) Catalog0
    {-# INLINE item #-}

newtype instance Catalog '[a] = Catalog1 a
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
-- | NB. This read instance is ambiguous, need type inference or annotations
-- to differentiate between other Catalog 'Read' instances.
instance (Distinct '[a], Read a) => Read (Catalog '[a]) where
    readPrec = readCatalog Catalog1
    {-# INLINE readPrec #-}
instance (Show a) => Show (Catalog '[a]) where
    showsPrec d (Catalog1 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance Cataloged '[a] where
    _Cataloged = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a]) where
    item = iso (\(Catalog1 t) -> t) Catalog1
    {-# INLINE item #-}
-- | 'AllItem' constraint results in UndecidableInstance. Safe because it just expands constraints.
instance (Distinct '[a], Item a t) => Project (Catalog '[a]) t where
    project f' t = fmap (\(Catalog1 a) -> t & item .~ a) (f' $ Catalog1 (t ^. item))
    {-# INLINE project #-}

newtype instance Catalog '[a, b] = Catalog2 (a, b)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
instance (Distinct '[a, b], Read a, Read b) => Read (Catalog '[a, b]) where
    readPrec = readCatalog Catalog2
    {-# INLINE readPrec #-}
instance (Show a, Show b) => Show (Catalog '[a, b]) where
    showsPrec d (Catalog2 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b]) => Cataloged '[a, b] where
    _Cataloged = iso (\(Catalog2 t) -> t) Catalog2
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b]) where
    item = iso (\(Catalog2 t) -> t) Catalog2 . _2
    {-# INLINE item #-}
instance (Distinct '[a, b], Item a t, Item b t) => Project (Catalog '[a, b]) t where
    project f' t = fmap (\(Catalog2 (a, b)) -> t & item .~ a & item .~ b)
                       (f' $ Catalog2 (t ^. item, t ^. item))
    {-# INLINE project #-}

newtype instance Catalog '[a, b, c] = Catalog3 (a, b, c)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Catalog '[a, b, c]) where
    readPrec = readCatalog Catalog3
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c) => Show (Catalog '[a, b, c]) where
    showsPrec d (Catalog3 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c]) => Cataloged '[a, b, c] where
    _Cataloged = iso (\(Catalog3 t) -> t) Catalog3
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c]) where
    item = iso (\(Catalog3 t) -> t) Catalog3 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c]) where
    item = iso (\(Catalog3 t) -> t) Catalog3 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c]) where
    item = iso (\(Catalog3 t) -> t) Catalog3 . _3
    {-# INLINE item #-}
instance (Distinct '[a, b, c], Item a t, Item b t, Item c t) => Project (Catalog '[a, b, c]) t where
    project f' t = fmap (\(Catalog3 (a, b, c)) -> t & item .~ a & item .~ b & item .~ c)
                       (f' $ Catalog3 (t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}

newtype instance Catalog '[a, b, c, d] = Catalog4 (a, b, c, d)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
instance (Distinct '[a, b, c, d], Read a, Read b, Read c, Read d) => Read (Catalog '[a, b, c, d]) where
    readPrec = readCatalog Catalog4
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c, Show d) => Show (Catalog '[a, b, c, d]) where
    showsPrec d (Catalog4 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c, d]) => Cataloged '[a, b, c, d] where
    _Cataloged = iso (\(Catalog4 t) -> t) Catalog4
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c, d]) where
    item = iso (\(Catalog4 t) -> t) Catalog4 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c, d]) where
    item = iso (\(Catalog4 t) -> t) Catalog4 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c, d]) where
    item = iso (\(Catalog4 t) -> t) Catalog4 . _3
    {-# INLINE item #-}
instance Item d (Catalog '[a, b, c, d]) where
    item = iso (\(Catalog4 t) -> t) Catalog4 . _4
    {-# INLINE item #-}
instance (Distinct '[a, b, c, d], Item a t, Item b t, Item c t, Item d t) => Project (Catalog '[a, b, c, d]) t where
    project f' t = fmap (\(Catalog4 (a, b, c, d)) -> t & item .~ a & item .~ b & item .~ c & item .~ d)
                       (f' $ Catalog4 (t ^. item, t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}


newtype instance Catalog '[a, b, c, d, e] = Catalog5 (a, b, c, d, e)
    deriving (Eq, Ord, Ix, Bounded, G.Generic)
instance (Distinct '[a, b, c, d, e], Read a, Read b, Read c, Read d, Read e) => Read (Catalog '[a, b, c, d, e]) where
    readPrec = readCatalog Catalog5
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c, Show d, Show e) => Show (Catalog '[a, b, c, d, e]) where
    showsPrec d (Catalog5 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c, d, e]) => Cataloged '[a, b, c, d, e] where
    _Cataloged = iso (\(Catalog5 t) -> t) Catalog5
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c, d, e]) where
    item = iso (\(Catalog5 t) -> t) Catalog5 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c, d, e]) where
    item = iso (\(Catalog5 t) -> t) Catalog5 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c, d, e]) where
    item = iso (\(Catalog5 t) -> t) Catalog5 . _3
    {-# INLINE item #-}
instance Item d (Catalog '[a, b, c, d, e]) where
    item = iso (\(Catalog5 t) -> t) Catalog5 . _4
    {-# INLINE item #-}
instance Item e (Catalog '[a, b, c, d, e]) where
    item = iso (\(Catalog5 t) -> t) Catalog5 . _5
    {-# INLINE item #-}
instance (Distinct '[a, b, c, d, e], Item a t, Item b t, Item c t, Item d t, Item e t) => Project (Catalog '[a, b, c, d, e]) t where
    project f' t = fmap (\(Catalog5 (a, b, c, d, e)) -> t & item .~ a & item .~ b & item .~ c & item .~ d & item .~ e)
                       (f' $ Catalog5 (t ^. item, t ^. item, t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}

-- | No Ix from here
newtype instance Catalog '[a, b, c, d, e, f] = Catalog6 (a, b, c, d, e, f)
    deriving (Eq, Ord, Bounded, G.Generic)
instance (Distinct '[a, b, c, d, e, f], Read a, Read b, Read c, Read d, Read e, Read f) => Read (Catalog '[a, b, c, d, e, f]) where
    readPrec = readCatalog Catalog6
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (Catalog '[a, b, c, d, e, f]) where
    showsPrec d (Catalog6 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c, d, e, f]) => Cataloged '[a, b, c, d, e, f] where
    _Cataloged = iso (\(Catalog6 t) -> t) Catalog6
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _3
    {-# INLINE item #-}
instance Item d (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _4
    {-# INLINE item #-}
instance Item e (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _5
    {-# INLINE item #-}
instance Item f (Catalog '[a, b, c, d, e, f]) where
    item = iso (\(Catalog6 t) -> t) Catalog6 . _6
    {-# INLINE item #-}
instance (Distinct '[a, b, c, d, e, f], Item a t, Item b t, Item c t, Item d t, Item e t, Item f t) => Project (Catalog '[a, b, c, d, e, f]) t where
    project f' t = fmap (\(Catalog6 (a, b, c, d, e, f)) -> t & item .~ a & item .~ b & item .~ c & item .~ d & item .~ e & item .~ f)
                       (f' $ Catalog6 (t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}

newtype instance Catalog '[a, b, c, d, e, f, g] = Catalog7 (a, b, c, d, e, f, g)
    deriving (Eq, Ord, Bounded, G.Generic)
instance (Distinct '[a, b, c, d, e, f, g], Read a, Read b, Read c, Read d, Read e, Read f, Read g) => Read (Catalog '[a, b, c, d, e, f, g]) where
    readPrec = readCatalog Catalog7
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (Catalog '[a, b, c, d, e, f, g]) where
    showsPrec d (Catalog7 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c, d, e, f, g]) => Cataloged '[a, b, c, d, e, f, g] where
    _Cataloged = iso (\(Catalog7 t) -> t) Catalog7
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _3
    {-# INLINE item #-}
instance Item d (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _4
    {-# INLINE item #-}
instance Item e (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _5
    {-# INLINE item #-}
instance Item f (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _6
    {-# INLINE item #-}
instance Item g (Catalog '[a, b, c, d, e, f, g]) where
    item = iso (\(Catalog7 t) -> t) Catalog7 . _7
    {-# INLINE item #-}
instance (Distinct '[a, b, c, d, e, f, g], Item a t, Item b t, Item c t, Item d t, Item e t, Item f t, Item g t) => Project (Catalog '[a, b, c, d, e, f, g]) t where
    project f' t = fmap (\(Catalog7 (a, b, c, d, e, f, g)) -> t & item .~ a & item .~ b & item .~ c & item .~ d & item .~ e & item .~ f & item .~ g)
                       (f' $ Catalog7 (t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}

newtype instance Catalog '[a, b, c, d, e, f, g, h] = Catalog8 (a, b, c, d, e, f, g, h)
    deriving (Eq, Ord, Bounded, G.Generic)
instance (Distinct '[a, b, c, d, e, f, g, h], Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h) => Read (Catalog '[a, b, c, d, e, f, g, h]) where
    readPrec = readCatalog Catalog8
    {-# INLINE readPrec #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (Catalog '[a, b, c, d, e, f, g, h]) where
    showsPrec d (Catalog8 t) = showCatalog d t
    {-# INLINE showsPrec #-}
instance (Distinct '[a, b, c, d, e, f, g, h]) => Cataloged '[a, b, c, d, e, f, g, h] where
    _Cataloged = iso (\(Catalog8 t) -> t) Catalog8
    {-# INLINE _Cataloged #-}
instance Item a (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _1
    {-# INLINE item #-}
instance Item b (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _2
    {-# INLINE item #-}
instance Item c (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _3
    {-# INLINE item #-}
instance Item d (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _4
    {-# INLINE item #-}
instance Item e (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _5
    {-# INLINE item #-}
instance Item f (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _6
    {-# INLINE item #-}
instance Item g (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _7
    {-# INLINE item #-}
instance Item h (Catalog '[a, b, c, d, e, f, g, h]) where
    item = iso (\(Catalog8 t) -> t) Catalog8 . _8
    {-# INLINE item #-}
instance (Distinct '[a, b, c, d, e, f, g, h], Item a t, Item b t, Item c t, Item d t, Item e t, Item f t, Item g t, Item h t) => Project (Catalog '[a, b, c, d, e, f, g, h]) t where
    project f' t = fmap (\(Catalog8 (a, b, c, d, e, f, g, h)) -> t & item .~ a & item .~ b & item .~ c & item .~ d & item .~ e & item .~ f & item .~ g & item .~ h)
                       (f' $ Catalog8 (t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item, t ^. item))
    {-# INLINE project #-}
