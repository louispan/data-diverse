{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Type.Internal where

import Data.Kind
import GHC.TypeLits

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type family PositionOfImpl (i :: Nat) x (xs :: [k]) :: Nat where
   PositionOfImpl i x (x ': xs) = i + 1
   PositionOfImpl i y (x ': xs) = PositionOfImpl (i + 1) y xs
   PositionOfImpl i x '[] = 0

-- | Get the first index of a type with exception on original search list
-- Modified from https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L223
type family IndexOfImpl (ctx :: [k]) x (xs :: [k]) :: Nat where
   IndexOfImpl ctx x (x ': xs) = 0
   IndexOfImpl ctx y (x ': xs) = 1 + IndexOfImpl ctx y xs
   IndexOfImpl ctx y '[] = TypeError ('Text "‘"
                                      ':<>: 'ShowType y
                                      ':<>: 'Text "’"
                                      ':<>: 'Text " is not a member of "
                                      ':<>: 'Text "‘"
                                      ':<>: 'ShowType ctx
                                      ':<>: 'Text "’")

-- | Add a type to a typelist, disallowing duplicates.
-- NB. xs are not checked.
type family InsertImpl (ctx :: [k]) (y :: k) (xs :: [k]) :: [k] where
    InsertImpl ctx y '[] = '[y]
    InsertImpl ctx  x (x ': xs) = TypeError ('Text "‘"
                                             ':<>: 'ShowType x
                                             ':<>: 'Text "’"
                                             ':<>: 'Text " is a duplicate in "
                                             ':<>: 'Text "‘"
                                             ':<>: 'ShowType ctx
                                             ':<>: 'Text "’")
    InsertImpl ctx  y (x ': xs) = x ': (InsertImpl ctx y xs)

type family OutcomeOfImpl (ctx :: [Type]) r (xs :: [Type]) :: Type where
    OutcomeOfImpl ctx r '[] = r
    OutcomeOfImpl ctx r ((a -> r) ': xs) = OutcomeOfImpl ctx r xs
    OutcomeOfImpl ctx r b = TypeError ('Text "‘"
                                       ':<>: 'ShowType r
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is not a result of all types in "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")

-- | Indexed access into the list
type family TypeAtImpl (orig :: Nat) (ctx :: [k]) (n :: Nat) (xs :: [k]) :: k where
   TypeAtImpl i ctx 0 '[] = TypeError ('Text "Index ‘"
                                       ':<>: 'ShowType i
                                       ':<>: 'Text "’"
                                       ':<>: 'Text " is out of bounds of "
                                       ':<>: 'Text "‘"
                                       ':<>: 'ShowType ctx
                                       ':<>: 'Text "’")
   TypeAtImpl i ctx 0 (x ': xs) = x
   TypeAtImpl i ctx n (x ': xs) = TypeAtImpl i ctx (n - 1) xs

type family ReverseImpl (ret :: [k]) (xs :: [k]) :: [k] where
    ReverseImpl ret '[] = ret
    ReverseImpl ret (x ': xs) = ReverseImpl (x ': ret) xs

type family WithoutImpl x (ret :: [k]) (xs :: [k]) :: [k] where
    WithoutImpl x ret '[] = ReverseImpl '[] ret
    WithoutImpl x ret (x ': xs) = WithoutImpl x ret xs
    WithoutImpl x ret (y ': xs) = WithoutImpl x (y ': ret) xs


type family SameLengthImpl (ctx :: [k]) (cty :: [k]) (xs :: [k]) (yx :: [k]) :: Constraint where
    SameLengthImpl as bs '[] '[] = ()
    SameLengthImpl as bs (x ': xs) (y ': ys) = SameLengthImpl as bs xs ys
    SameLengthImpl as bs xs ys = TypeError ('Text "‘"
                                            ':<>: 'ShowType as
                                            ':<>: 'Text "’"
                                            ':<>: 'Text " is not the same length as "
                                            ':<>: 'Text "‘"
                                            ':<>: 'ShowType bs
                                            ':<>: 'Text "’")
type family InitImpl (ret :: [k]) (xs :: [k]) :: [k] where
    InitImpl ret '[]  = TypeError ('Text "Cannot Init an empty type list")
    InitImpl ret '[x] = ReverseImpl '[] ret
    InitImpl ret (x ': xs) = InitImpl (x ': ret) xs
