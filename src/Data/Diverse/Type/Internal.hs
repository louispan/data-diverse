{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Type.Internal where

import Data.Kind
import GHC.TypeLits

-- | Get the first position of a type (indexed by 1)
-- Will return 0 if @x@ doesn't exists in @xs@.
type family PositionOfImpl (i :: Nat) (x :: k) (xs :: [k]) :: Nat where
   PositionOfImpl i x (x ': xs) = i + 1
   PositionOfImpl i y (x ': xs) = PositionOfImpl (i + 1) y xs
   PositionOfImpl i x '[] = 0

-- | Get the first index of a type from a list
type family IndexOfImpl (ctx :: [k]) (x :: k) (xs :: [k]) :: Nat where
   IndexOfImpl ctx x (x ': xs) = 0
   IndexOfImpl ctx y (x ': xs) = 1 + IndexOfImpl ctx y xs
   IndexOfImpl ctx y '[] = TypeError ('Text "IndexOf error: ‘"
                                      ':<>: 'ShowType y
                                      ':<>: 'Text "’"
                                      ':<>: 'Text " is not a member of "
                                      ':<>: 'Text "‘"
                                      ':<>: 'ShowType ctx
                                      ':<>: 'Text "’")

-- | Searches for y in ys
-- if not found, than use y, and repeat search with next (y ': ys) in ctx
-- else if found, then don't use y, then repeat search with next (y ': ys) in ctx
type family DistinctImpl (ctx :: [k]) (y :: k) (ys :: [k]) :: [k] where
    DistinctImpl '[] y '[] = y ': '[]
    DistinctImpl '[] y (y ': xs) = '[]
    DistinctImpl (x ': xs) y '[] = y ': DistinctImpl xs x xs
    DistinctImpl (x ': xs) y (y ': ys) = DistinctImpl xs x xs
    DistinctImpl ctx y (x ': xs) = DistinctImpl ctx y xs

-- | Errors if a type exists in a typelist
type family MissingImpl (ctx :: [k]) (y :: k) (xs :: [k]) :: Constraint where
    MissingImpl ctx y '[] = ()
    MissingImpl ctx x (x ': xs) = TypeError ('Text "Missing error: ‘"
                                             ':<>: 'ShowType x
                                             ':<>: 'Text "’"
                                             ':<>: 'Text " is a duplicate in "
                                             ':<>: 'Text "‘"
                                             ':<>: 'ShowType ctx
                                             ':<>: 'Text "’")
    MissingImpl ctx y (x ': xs) = (MissingImpl ctx y xs)

-- | Ensures that the type list contain unique types
type family IsDistinctImpl (ctx :: [k]) (xs :: [k]) :: Constraint where
    IsDistinctImpl ctx '[] = ()
    IsDistinctImpl ctx (x ': xs) = (MissingImpl ctx x xs, IsDistinctImpl ctx xs)

-- | Ensures that @x@ only ever appears once in @xs@
type family UniqueImpl (ctx :: [k]) (x :: k) (xs :: [k]) :: Constraint where
    UniqueImpl ctx x '[] = ()
    UniqueImpl ctx x (x ': xs) = MissingImpl ctx x xs
    UniqueImpl ctx x (y ': xs) = UniqueImpl ctx x xs

-- | Indexed access into the list
type family KindAtIndexImpl (orig :: Nat) (ctx :: [k]) (n :: Nat) (xs :: [k]) :: k where
    KindAtIndexImpl i ctx 0 '[] = TypeError ('Text "KindAtIndex error: Index ‘"
                                       ':<>: 'ShowType i
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is out of bounds of "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")
    KindAtIndexImpl i ctx 0 (x ': xs) = x
    KindAtIndexImpl i ctx n (x ': xs) = KindAtIndexImpl i ctx (n - 1) xs

-- | Labelled access into the list
type family KindAtLabelImpl (l :: k1) (ctx :: [k2]) (xs :: [k2]) :: k2 where
    KindAtLabelImpl l ctx '[] = TypeError ('Text "KindAtLabel error: Label ‘"
                                       ':<>: 'ShowType l
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is not found in "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")
    KindAtLabelImpl l ctx (tagged l x ': xs) = tagged l x
    KindAtLabelImpl l ctx (x ': xs) = KindAtLabelImpl l ctx xs

-- | Ensures two typelists are the same length
type family SameLengthImpl (ctx :: [k1]) (cty :: [k2]) (xs :: [k1]) (yx :: [k2]) :: Constraint where
    SameLengthImpl as bs '[] '[] = ()
    SameLengthImpl as bs (x ': xs) (y ': ys) = SameLengthImpl as bs xs ys
    SameLengthImpl as bs xs ys = TypeError ('Text "SameLength error: ‘"
                                            ':<>: 'ShowType as
                                            ':<>: 'Text "’"
                                            ':<>: 'Text " is not the same length as "
                                            ':<>: 'Text "‘"
                                            ':<>: 'ShowType bs
                                            ':<>: 'Text "’")

-- | The typelist @xs@ without the type at Nat @n@. @n@ must be within bounds of @xs@
type family WithoutIndexImpl (i :: Nat) (ctx :: [k]) (n :: Nat) (xs :: [k]) :: [k] where
    WithoutIndexImpl i ctx n '[] = TypeError ('Text "WithoutIndex error: Index ‘"
                                       ':<>: 'ShowType i
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is out of bounds of "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")
    WithoutIndexImpl i ctx 0 (x ': xs) = xs
    WithoutIndexImpl i ctx n (x ': xs) = x ': WithoutIndexImpl i ctx (n - 1) xs

-- | The typelist @xs@ without the type at Nat @n@ replaced by @y@. @n@ must be within bounds of @xs@
type family ReplaceIndexImpl (i :: Nat) (ctx :: [k]) (n :: Nat) (y :: k) (xs :: [k]) :: [k] where
    ReplaceIndexImpl i ctx n y '[] = TypeError ('Text "ReplaceIndex error: Index ‘"
                                       ':<>: 'ShowType i
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is out of bounds of "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")
    ReplaceIndexImpl i ctx 0 y (x ': xs) = y ': xs
    ReplaceIndexImpl i ctx n y (x ': xs) = x ': ReplaceIndexImpl i ctx (n - 1) y xs

-- | The typelist @xs@ with the first @x@ replaced by @y@. It is okay for @x@ not to exist in @xs@
type family ReplaceImpl (x :: k) (y :: k) (xs :: [k]) :: [k] where
    ReplaceImpl x y '[] = '[]
    ReplaceImpl x y (x ': xs) = y ': xs
    ReplaceImpl x y (z ': xs) = z ': ReplaceImpl x y xs

-- | The typelist @zs@ with the first @xs@ replaced by @ys@.
-- @xs@ must be the same size as @ys@
type family ReplacesImpl (xs' :: [k]) (ys' :: [k]) (xs :: [k]) (ys :: [k]) (zs :: [k]) :: [k] where
    ReplacesImpl xs' ys' xs ys '[] = '[]
    ReplacesImpl xs' ys' '[] '[] (z ': zs) = z ': ReplacesImpl xs' ys' xs' ys' zs
    ReplacesImpl xs' ys' (x ': xs) (y ': ys) (x ': zs) = y ': ReplacesImpl xs' ys' xs' ys' zs
    ReplacesImpl xs' ys' (x ': xs) (y ': ys) (z ': zs) = ReplacesImpl xs' ys' xs ys (z ': zs)
    ReplacesImpl xs' ys' xs ys zs = TypeError ('Text "Replaces error: ‘"
                                       ':<>: 'ShowType xs'
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " must be the same size as "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ys'
                                       ':<>: 'Text "’")

-- | Zips up @xs@ and @ys@, which must be the same length
type family ZipImpl (xs' :: [k]) (ys' :: [k]) (xs :: [k]) (ys :: [k]) :: [k] where
    ZipImpl xs' ys' '[] '[] = '[]
    ZipImpl xs' ys' (x ': xs) (y ': ys) = (x, y) ': ZipImpl xs' ys' xs ys
    ZipImpl xs' ys' xs ys = TypeError ('Text "Zip error: ‘"
                              ':<>: 'ShowType xs'
                              ':<>: 'Text "’"
                              ':<>: 'Text " must be the same size as "
                              ':<>: 'Text "‘"
                              ':<>: 'ShowType ys'
                              ':<>: 'Text "’")
