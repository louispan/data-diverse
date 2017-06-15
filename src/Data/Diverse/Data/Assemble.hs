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

-- | Undecidable instances! But this is safe since it's a wrapper
newtype Assemble g (xs :: [Type]) r = Assemble (g xs r)

-- | null case that doesn't even use the Emit
instance ( Emit g '[] r
         , AFoldable (Assemble g '[]) r
         ) =>
         AFoldable (Assemble g '[]) r where
    afoldr _ z _ = z

instance ( Emit g (x ': xs) r
         , Reiterate g (x ': xs)
         , AFoldable (Assemble g xs) r
         ) =>
         AFoldable (Assemble g (x ': xs)) r where
    afoldr f z (Assemble g) = f (emit g) (afoldr f z (Assemble (reiterate g)))
