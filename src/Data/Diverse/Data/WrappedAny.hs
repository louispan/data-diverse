module Data.Diverse.Data.WrappedAny where

import GHC.Prim (Any)

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny { unwrappedAny :: Any }
