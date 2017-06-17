{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Collector where

import Data.Diverse.AFoldable
import Data.Diverse.Emit
import Data.Diverse.Reiterate
import Data.Kind

-- | Folds output from Emitter while iterating the typelist.
-- This version guaranteeds that the @Emit '[]@ is not instantiated.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector g (xs :: [Type]) r = Collector (g xs r)

-- | null case that doesn't even use the Emit
instance ( AFoldable (Collector g '[]) r
         ) =>
         AFoldable (Collector g '[]) r where
    afoldr _ z _ = z

instance ( Emit g (x ': xs) r
         , Reiterate g (x ': xs)
         , AFoldable (Collector g xs) r
         ) =>
         AFoldable (Collector g (x ': xs)) r where
    afoldr f z (Collector g) = f (emit g) (afoldr f z (Collector (reiterate g)))


-- | Folds output from Emitter while iterating the typelist.
-- This version uses the @Emit '[]@ instance.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Collector0 g (xs :: [Type]) r = Collector0 (g xs r)

-- | terminating case that does use emit
instance ( Emit g '[] r
         , AFoldable (Collector0 g '[]) r
         ) =>
         AFoldable (Collector0 g '[]) r where
    afoldr f z (Collector0 g) = f (emit g) z

instance ( Emit g (x ': xs) r
         , Reiterate g (x ': xs)
         , AFoldable (Collector0 g xs) r
         ) =>
         AFoldable (Collector0 g (x ': xs)) r where
    afoldr f z (Collector0 g) = f (emit g) (afoldr f z (Collector0 (reiterate g)))
