{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel.Internal where

import Data.Kind
import GHC.TypeLits

-- | Get the first index of a type with exception on original search list
-- Modified from https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L223
type family IndexOfImpl (ctx :: [Type]) x (xs :: [Type]) :: Nat where
   IndexOfImpl ctx x (x ': xs) = 0
   IndexOfImpl ctx y (x ': xs) = 1 + IndexOfImpl ctx y xs
   IndexOfImpl ctx y '[]       = TypeError ( 'Text "‘"
                                           ':<>: 'ShowType y
                                           ':<>: 'Text "’"
                                           ':<>: 'Text " is not a member of "
                                           ':<>: 'Text "‘"
                                           ':<>: 'ShowType ctx
                                           ':<>: 'Text "’")

-- | Add a type to a typelist, disallowing duplicates.
-- NB. xs are not checked.
type family InsertImpl (ctx :: [Type]) (y :: Type) (xs :: [Type]) :: [Type] where
    -- empty case
    InsertImpl ctx y '[] = '[y]
    -- case when the type matched the head
    InsertImpl ctx  x (x ': xs) = TypeError ('Text "‘"
                                           ':<>: 'ShowType x
                                           ':<>: 'Text "’"
                                           ':<>: 'Text " is a duplicate in "
                                           ':<>: 'Text "‘"
                                           ':<>: 'ShowType ctx
                                           ':<>: 'Text "’")
    -- recurse if the type doesn't match the head
    InsertImpl ctx  y (x ': xs) = x ': (InsertImpl ctx y xs)

type family CaseResultImpl (ctx :: [Type]) r (xs :: [Type]) :: Type where
    CaseResultImpl ctx r '[] = r
    CaseResultImpl ctx r ((a -> r) ': xs) = CaseResultImpl ctx r xs
    CaseResultImpl ctx r b = TypeError ('Text "‘"
                                    ':<>: 'ShowType r
                                    ':<>: 'Text "’"
                                    ':<>: 'Text " is not a result of all types in "
                                    ':<>: 'Text "‘"
                                    ':<>: 'ShowType ctx
                                    ':<>: 'Text "’")

-- -- | Modified from https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L175
-- type family IsSubsetEx (ctx :: [Type]) smaller larger :: Bool where
--    IsSubsetEx ctx x x = 'True
--    IsSubsetEx ctx '[] l = 'True
--    IsSubsetEx ctx s '[] = TypeError ('Text "‘"
--                                     ':<>: 'ShowType s
--                                     ':<>: 'Text "’"
--                                     ':<>: 'Text " is not a subset of "
--                                     ':<>: 'Text "‘"
--                                     ':<>: 'ShowType ctx
--                                     ':<>: 'Text "’")
--    IsSubsetEx ctx (x ': xs) (x ': ys) = IsSubsetEx ctx xs ctx
--    IsSubsetEx ctx (x ': xs) (y ': ys) = IsSubsetEx ctx (x ': xs) ys

-- -- | Check that a type is member of a type list
-- -- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L158
-- type family IsMemberEx (ctx :: [Type]) x xs :: Bool where
--    IsMemberEx ctx x (x ': xs) = 'True
--    IsMemberEx ctx x (y ': xs) = IsMemberEx ctx x xs
--    IsMemberEx ctx x '[] = TypeError ('Text "‘"
--                                     ':<>: 'ShowType x
--                                     ':<>: 'Text "’"
--                                     ':<>: 'Text " is not a member of "
--                                     ':<>: 'Text "‘"
--                                     ':<>: 'ShowType ctx
--                                     ':<>: 'Text "’")

-- | Indexed access into the list
type family TypeAtImpl (orig :: Nat) (ctx :: [Type]) (n :: Nat) (xs :: [Type]) :: Type where
   TypeAtImpl i ctx 0 '[] = TypeError ('Text "Index ‘"
                                   ':<>: 'ShowType i
                                   ':<>: 'Text "’"
                                   ':<>: 'Text " is out of bounds of "
                                   ':<>: 'Text "‘"
                                   ':<>: 'ShowType ctx
                                   ':<>: 'Text "’")
   TypeAtImpl i ctx 0 (x ': xs) = x
   TypeAtImpl i ctx n (x ': xs) = TypeAtImpl i ctx (n - 1) xs

type family ReverseImpl (ok :: [Type]) (xs :: [Type]) :: [Type] where
    ReverseImpl ok '[] = ok
    ReverseImpl ok (x ': xs) = ReverseImpl (x ': ok) xs

type family WithoutImpl x (ok :: [Type]) (xs :: [Type]) :: [Type] where
    WithoutImpl x ok '[] = ReverseImpl '[] ok
    WithoutImpl x ok (x ': xs) = WithoutImpl x ok xs
    WithoutImpl x ok (y ': xs) = WithoutImpl x (y ': ok) xs


type family SameLengthImpl (ctx :: [Type]) (cty :: [Type]) (xs :: [Type]) (yx :: [Type]) :: Constraint where
    SameLengthImpl as bs '[] '[] = ()
    SameLengthImpl as bs (x ': xs) (y ': ys) = SameLengthImpl as bs xs ys
    SameLengthImpl as bs xs ys = TypeError ('Text "‘"
                                   ':<>: 'ShowType as
                                   ':<>: 'Text "’"
                                   ':<>: 'Text " is not the same length as "
                                   ':<>: 'Text "‘"
                                   ':<>: 'ShowType bs
                                   ':<>: 'Text "’")
