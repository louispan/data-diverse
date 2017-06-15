{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.Class.AFoldable where

-- | Constrained Foldable for a specified type instead for all types.
class AFoldable t a where
     afoldr :: (a -> b -> b) -> b -> t a -> b

afoldl' :: AFoldable t a => (b -> a -> b) -> b -> t a -> b
afoldl' f z0 xs = afoldr f' id xs z0
  where f' x k z = k $! f z x
