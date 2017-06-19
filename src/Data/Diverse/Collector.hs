{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Collector where

import Data.Diverse.AFoldable
import Data.Diverse.Emit
import Data.Diverse.Reiterate
import Data.Kind
import GHC.TypeLits

-- | Folds output from Emitter while iterating the typelist.
-- This version guaranteeds that the @Emit '[]@ is not instantiated.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector e (xs :: [Type]) r = Collector (e xs r)

-- | null case that doesn't even use the Emit
instance AFoldable (Collector e '[]) r where
    afoldr _ z _ = z

instance ( Emit e (x ': xs) r
         , Reiterate e (x ': xs)
         , AFoldable (Collector e xs) r
         ) =>
         AFoldable (Collector e (x ': xs)) r where
    afoldr f z (Collector e) = f (emit e) (afoldr f z (Collector (reiterate e)))

-- | Folds output from Emitter while iterating the typelist.
-- This version uses the @Emit '[]@ instance.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector0 e (xs :: [Type]) r = Collector0 (e xs r)

-- | terminating case that does use emit
instance (Emit e '[] r) =>
         AFoldable (Collector0 e '[]) r where
    afoldr f z (Collector0 e) = f (emit e) z

instance ( Emit e (x ': xs) r
         , Reiterate e (x ': xs)
         , AFoldable (Collector0 e xs) r
         ) =>
         AFoldable (Collector0 e (x ': xs)) r where
    afoldr f z (Collector0 e) = f (emit e) (afoldr f z (Collector0 (reiterate e)))

--------------------------------------------

-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorN e (n :: Nat) (xs :: [Type]) r = CollectorN (e n xs r)

-- | null case that doesn't even use the Emit
instance AFoldable (CollectorN e n '[]) r where
    afoldr _ z _ = z

instance ( Emit (e n) (x ': xs) r
         , ReiterateN e n (x ': xs)
         , AFoldable (CollectorN e (n + 1) xs) r
         ) =>
         AFoldable (CollectorN e n (x ': xs)) r where
    afoldr f z (CollectorN e) = f (emit e) (afoldr f z (CollectorN (reiterateN e)))

-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorN0 e (n :: Nat) (xs :: [Type]) r = CollectorN0 (e n xs r)

-- | terminating case that does use emit
instance (Emit (e n) '[] r) =>
         AFoldable (CollectorN0 e n '[]) r where
    afoldr f z (CollectorN0 e) = f (emit e) z

instance ( Emit (e n) (x ': xs) r
         , ReiterateN e n (x ': xs)
         , AFoldable (CollectorN0 e (n + 1) xs) r
         ) =>
         AFoldable (CollectorN0 e n (x ': xs)) r where
    afoldr f z (CollectorN0 e) = f (emit e) (afoldr f z (CollectorN0 (reiterateN e)))


--------------------------------------------

-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorL e (ls :: [k]) (xs :: [Type]) r = CollectorL (e ls xs r)

-- | null case that doesn't even use the Emit
instance AFoldable (CollectorL e '[] '[]) r where
    afoldr _ z _ = z

instance ( Emit (e (l ': ls)) (x ': xs) r
         , ReiterateL e (l ': ls) (x ': xs)
         , AFoldable (CollectorL e ls xs) r
         ) =>
         AFoldable (CollectorL e (l ': ls) (x ': xs)) r where
    afoldr f z (CollectorL e) = f (emit e) (afoldr f z (CollectorL (reiterateL e)))

-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorL0 e (ls :: [k]) (xs :: [Type]) r = CollectorL0 (e ls xs r)

-- | terminating case that does use emit
instance (Emit (e '[]) '[] r) =>
         AFoldable (CollectorL0 e '[] '[]) r where
    afoldr f z (CollectorL0 e) = f (emit e) z

instance ( Emit (e (l ': ls)) (x ': xs) r
         , ReiterateL e (l ': ls) (x ': xs)
         , AFoldable (CollectorL0 e ls xs) r
         ) =>
         AFoldable (CollectorL0 e (l ': ls) (x ': xs)) r where
    afoldr f z (CollectorL0 e) = f (emit e) (afoldr f z (CollectorL0 (reiterateL e)))
