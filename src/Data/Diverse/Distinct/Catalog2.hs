-- | Re-export Catalog without the constructor
module Data.Diverse.Distinct.Catalog2
    ( Catalog -- ^ Hide constructor
    , blank
    , singleton
    , cons
    , (./)
    , snoc
    , (\.)
    , append
    , (/./)
    , front
    , back
    , aft
    , fore
    , fetch
    , replace
    , Via -- ^ no constructor
    , via -- ^ safe construction
    , forCatalog
    , collect
    , Cases(..)
    , narrow
    , amend
    ) where

import Data.Diverse.Distinct.Catalog2.Internal
import Prelude hiding (tail, init)
