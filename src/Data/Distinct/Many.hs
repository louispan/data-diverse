{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.Many where

import Data.Distinct.Many.Internal
-- import Prelude hiding (head)
import Unsafe.Coerce

-- import Data.Proxy
-- import GHC.Exts (Constraint)

-- -- | Get the value if it has the indexed type
-- getManyN :: forall (n :: Nat) (l :: [*]).
--    ( KnownNat n
--    ) => Many l -> Maybe (Index n l)
-- {-# INLINE getManyN #-}
-- getManyN (Many t a) = do
--    guard (t == natValue @n)
--    return (unsafeCoerce a) -- we know it is the effective type

-- | Try to pick the head of a variant value,
-- and return either the Right value,
-- or the Left-overs.
-- This can be used continually to get to the variant type.
pick :: Many (x ': xs) -> Either (Many xs) x
pick (Many 0 x) = Right (unsafeCoerce x)
pick (Many n x) = Left (Many (n - 1) x)

-- -- | Set the first matching type of a Variant
-- getVariant :: forall a l.
--    ( Member a l
--    ) => Many xs -> Maybe a
-- {-# INLINE getVariant #-}
-- getVariant = getVariantN @(IndexOf a l)
