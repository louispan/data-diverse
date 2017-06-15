{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Data.Assemble where

import Data.Diverse.Class.AFoldable
import Data.Diverse.Class.Emit
import Data.Diverse.Class.Reiterate
import Data.Kind

-- | Folds output from Emitter while iterating the typelist.
-- This version guaranteeds that the @Emit '[]@ is not instantiated.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Assemble g (xs :: [Type]) r = Assemble (g xs r)

-- | null case that doesn't even use the Emit
instance ( AFoldable (Assemble g '[]) r
         ) =>
         AFoldable (Assemble g '[]) r where
    afoldr _ z _ = z

instance ( Emit g (x ': xs) r
         , Reiterate g (x ': xs)
         , AFoldable (Assemble g xs) r
         ) =>
         AFoldable (Assemble g (x ': xs)) r where
    afoldr f z (Assemble g) = f (emit g) (afoldr f z (Assemble (reiterate g)))


-- | Folds output from Emitter while iterating the typelist.
-- This version uses the @Emit '[]@ instance.
-- Undecidable instances! But this is safe since it's a wrapper
newtype Assemble0 g (xs :: [Type]) r = Assemble0 (g xs r)

-- | terminating case that does use emit
instance ( Emit g '[] r
         , AFoldable (Assemble0 g '[]) r
         ) =>
         AFoldable (Assemble0 g '[]) r where
    afoldr f z (Assemble0 g) = f (emit g) z

instance ( Emit g (x ': xs) r
         , Reiterate g (x ': xs)
         , AFoldable (Assemble0 g xs) r
         ) =>
         AFoldable (Assemble0 g (x ': xs)) r where
    afoldr f z (Assemble0 g) = f (emit g) (afoldr f z (Assemble0 (reiterate g)))
