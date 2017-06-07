{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel.Internal where

import GHC.TypeLits

-- | Get the first index of a type with exception on original search list
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L223
type family IndexOfEx (ctx :: [*]) a (l :: [*]) :: Nat where
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
type family InsertEx (ctx :: [*]) (xs :: [*]) (y :: *) :: [*] where
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
type family UnionEx (ctx :: [*]) (xs :: [*]) (ys :: [*]) :: [*] where
    -- empty case
    UnionEx ctx '[] '[] = '[]
    UnionEx ctx xs '[] = xs
    UnionEx ctx xs (y ': ys) = UnionEx ctx (InsertEx ctx xs y) ys
