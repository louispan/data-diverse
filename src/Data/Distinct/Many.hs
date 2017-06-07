{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.Many where

import Data.Distinct.Many.Internal
import GHC.Prim (Any)
import Prelude hiding (head)
import Unsafe.Coerce

-- import Data.Proxy
-- import GHC.Exts (Constraint)

-- | Pick the head of a variant value
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Many.hs#L293
-- Also see Data.HList.Many.spliMany1'
head :: Many (x ': xs) -> Either (Many xs) x
head (Many 0 x) = Right (unsafeCoerce x)
head (Many n x) = Left (Many (n-1) x)

-- -- | Get the value if it has the indexed type
-- getManyN :: forall (n :: Nat) (l :: [*]).
--    ( KnownNat n
--    ) => Many l -> Maybe (Index n l)
-- {-# INLINE getManyN #-}
-- getManyN (Many t a) = do
--    guard (t == natValue @n)
--    return (unsafeCoerce a) -- we know it is the effective type

-- | only internally used
index' :: Many a -> Word
index' (Many n _) = n
