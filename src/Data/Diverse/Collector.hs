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

-- | Folds output from an 'Emit'ter of values while __'reiterate'__ing the typelist.
-- This version guaranteeds that the @Emit e '[]@ is not instantiated.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector e (xs :: [Type]) r = Collector (e xs r)

-- | null case that doesn't even use 'emit', so that an instance of @Emit e '[]@ is not needed.
instance AFoldable (Collector e '[]) r where
    afoldr _ z _ = z

-- | Folds values by 'reiterate' 'Emit'ters through the @xs@ typelist.
instance ( Emit e (x ': xs) r
         , Reiterate e (x ': xs)
         , AFoldable (Collector e xs) r
         ) =>
         AFoldable (Collector e (x ': xs)) r where
    afoldr f z (Collector e) = f (emit e) (afoldr f z (Collector (reiterate e)))

-- | Folds output from Emitter while __'reiterate'__ing the typelist.
-- This version uses the @Emit e '[]@ instance.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector0 e (xs :: [Type]) r = Collector0 (e xs r)

-- | terminating case that does use @Emit e '[]@
instance (Emit e '[] r) =>
         AFoldable (Collector0 e '[]) r where
    afoldr f z (Collector0 e) = f (emit e) z

-- | Folds values by 'reiterate' 'Emit'ters through the @xs@ typelist.
instance ( Emit e (x ': xs) r
         , Reiterate e (x ': xs)
         , AFoldable (Collector0 e xs) r
         ) =>
         AFoldable (Collector0 e (x ': xs)) r where
    afoldr f z (Collector0 e) = f (emit e) (afoldr f z (Collector0 (reiterate e)))

--------------------------------------------

-- | Folds output from an 'Emit'ter of values while __'reiterateN'__ing the typelist.
-- This version guaranteeds that the @Emit (e n) '[]@ is not instantiated.
-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorN e (n :: Nat) (xs :: [Type]) r = CollectorN (e n xs r)

-- | null case that doesn't even use 'emit', so that an instance of @Emit (e n) '[]@ is not needed.
instance AFoldable (CollectorN e n '[]) r where
    afoldr _ z _ = z

-- | Folds values by 'reiterateN' 'Emit'ters through the @xs@ typelist.
instance ( Emit (e n) (x ': xs) r
         , ReiterateN e n (x ': xs)
         , AFoldable (CollectorN e (n + 1) xs) r
         ) =>
         AFoldable (CollectorN e n (x ': xs)) r where
    afoldr f z (CollectorN e) = f (emit e) (afoldr f z (CollectorN (reiterateN e)))

-- | Folds output from Emitter while __'reiterateN'__ing the typelist.
-- This version uses the @Emit e '[]@ instance.
-- Undecidable instances! But this is safe since it's a wrapper
newtype CollectorN0 e (n :: Nat) (xs :: [Type]) r = CollectorN0 (e n xs r)

-- | terminating case that does use @Emit (e n) '[]@
instance (Emit (e n) '[] r) =>
         AFoldable (CollectorN0 e n '[]) r where
    afoldr f z (CollectorN0 e) = f (emit e) z

-- | Folds values by 'reiterateN' 'Emit'ters through the @xs@ typelist.
instance ( Emit (e n) (x ': xs) r
         , ReiterateN e n (x ': xs)
         , AFoldable (CollectorN0 e (n + 1) xs) r
         ) =>
         AFoldable (CollectorN0 e n (x ': xs)) r where
    afoldr f z (CollectorN0 e) = f (emit e) (afoldr f z (CollectorN0 (reiterateN e)))
