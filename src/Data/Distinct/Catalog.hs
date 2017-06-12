-- | Re-export Catalog without the constructor
module Data.Distinct.Catalog
    ( Catalog
    , Cataloged(..)
    , catalog
    , toTuple
    , Item(..)
    , Items
    , fetch
    , replace
    , Project(..)
    , narrow
    , amend
    ) where

import Data.Distinct.Catalog.Internal
