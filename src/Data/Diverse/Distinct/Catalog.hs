-- | Re-export Catalog without the constructor
module Data.Diverse.Distinct.Catalog
    ( Catalog -- ^ Hide constructor
    , Cataloged(..)
    , catalog
    , toTuple
    , Item(..)
    , AllItem
    , fetch
    , replace
    , Project(..)
    , projected
    , narrow
    , amend
    ) where

import Data.Diverse.Distinct.Catalog.Internal
