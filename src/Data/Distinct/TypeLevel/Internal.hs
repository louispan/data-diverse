{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel.Internal where

import Data.Kind
import GHC.TypeLits

-- | Get the first index of a type with exception on original search list
-- Modified from https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L223
type family IndexOfEx (ctx :: [Type]) x (xs :: [Type]) :: Nat where
   IndexOfEx ctx x (x ': xs) = 0
   IndexOfEx ctx y (x ': xs) = 1 + IndexOfEx ctx y xs
   IndexOfEx ctx y '[]       = TypeError ( 'Text "‘"
                                           ':<>: 'ShowType y
                                           ':<>: 'Text "’"
                                           ':<>: 'Text " is not a member of "
                                           ':<>: 'Text "‘"
                                           ':<>: 'ShowType ctx
                                           ':<>: 'Text "’")

-- | Add a type to a typelist, disallowing duplicates.
-- NB. xs are not checked.
type family InsertEx (ctx :: [Type]) (xs :: [Type]) (y :: Type) :: [Type] where
    -- empty case
    InsertEx ctx '[] y = '[y]
    -- case when the type matched the head
    InsertEx ctx (x ': xs) x = TypeError ( 'Text "‘"
                                           ':<>: 'ShowType x
                                           ':<>: 'Text "’"
                                           ':<>: 'Text " is a duplicate in "
                                           ':<>: 'Text "‘"
                                           ':<>: 'ShowType ctx
                                           ':<>: 'Text "’")
    -- recurse if the type doesn't match the head
    InsertEx ctx (x ': xs) y = x ': (InsertEx ctx xs y)

-- | Combine two type lists together, assuming disallowing duplicates from ys
-- NB. xs are not checked.
type family UnionEx (ctx :: [Type]) (xs :: [Type]) (ys :: [Type]) :: [Type] where
    -- empty case
    UnionEx ctx '[] '[] = '[]
    UnionEx ctx xs '[] = xs
    UnionEx ctx xs (y ': ys) = UnionEx ctx (InsertEx ctx xs y) ys


-- type family SwitchResultEx (ctx :: [Type]) r (xs :: [Type]) :: Type where
--     SwitchResultEx ctx r '[] = r
--     SwitchResultEx ctx r ((a -> r) ': xs) = SwitchResultEx ctx r xs
--     SwitchResultEx ctx r b = TypeError ( 'Text "‘"
--                                     ':<>: 'ShowType r
--                                     ':<>: 'Text "’"
--                                     ':<>: 'Text " is not a result of all types in "
--                                     ':<>: 'Text "‘"
--                                     ':<>: 'ShowType ctx
--                                     ':<>: 'Text "’")

-- -- | Modified from https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L175
-- type family IsSubsetEx (ctx :: [Type]) smaller larger :: Bool where
--    IsSubsetEx ctx x x = 'True
--    IsSubsetEx ctx '[] l = 'True
--    IsSubsetEx ctx s '[] = TypeError ( 'Text "‘"
--                                     ':<>: 'ShowType s
--                                     ':<>: 'Text "’"
--                                     ':<>: 'Text " is not a subset of "
--                                     ':<>: 'Text "‘"
--                                     ':<>: 'ShowType ctx
--                                     ':<>: 'Text "’")
--    IsSubsetEx ctx (x ': xs) (x ': ys) = IsSubsetEx ctx xs ctx
--    IsSubsetEx ctx (x ': xs) (y ': ys) = IsSubsetEx ctx (x ': xs) ys

-- | Check that a type is member of a type list
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L158
type family IsMemberEx (ctx :: [Type]) x xs :: Bool where
   IsMemberEx ctx x (x ': xs) = 'True
   IsMemberEx ctx x (y ': xs) = IsMemberEx ctx x xs
   IsMemberEx ctx x '[] = TypeError ( 'Text "‘"
                                    ':<>: 'ShowType x
                                    ':<>: 'Text "’"
                                    ':<>: 'Text " is not a member of "
                                    ':<>: 'Text "‘"
                                    ':<>: 'ShowType ctx
                                    ':<>: 'Text "’")

-- | Indexed access into the list
type family TypeAtEx (orig :: Nat) (ctx :: [Type]) (n :: Nat) (xs :: [Type]) where
   TypeAtEx i ctx 0 '[] = TypeError ( 'Text "Index ‘"
                                   ':<>: 'ShowType i
                                   ':<>: 'Text "’"
                                   ':<>: 'Text " is out of bounds of "
                                   ':<>: 'Text "‘"
                                   ':<>: 'ShowType ctx
                                   ':<>: 'Text "’")
   TypeAtEx i ctx 0 (x ': xs) = x
   TypeAtEx i ctx n (x ': xs) = TypeAtEx i ctx (n - 1) xs

type family DoReverse (ok :: [Type]) (xs :: [Type]) :: [Type] where
    DoReverse ok '[] = ok
    DoReverse ok (x ': xs) = DoReverse (x ': ok) xs

type family DoWithout x (ok :: [Type]) (xs :: [Type]) :: [Type] where
    DoWithout x ok '[] = DoReverse '[] ok
    DoWithout x ok (x ': xs) = DoWithout x ok xs
    DoWithout x ok (y ': xs) = DoWithout x (y ': ok) xs
