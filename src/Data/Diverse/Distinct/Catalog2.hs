-- | Re-export Catalog without the constructor
module Data.Diverse.Distinct.Catalog2
    ( Catalog -- ^ Hide constructor
    , null
    , singleton
    , cons
    , (./)
    , snoc
    , (\.)
    , append
    , (/./)
    , head
    , last
    , tail
    , init
    , lookup
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
import Prelude hiding (null, head, last, tail, init, lookup)
