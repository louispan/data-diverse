-- | Re-export Catalog without the constructor
module Data.Distinct.Catalog
    ( Catalog
    , catalog
    , _Cataloged
    , _Cataloged'
    , _Uncataloged
    , _Uncataloged'
    , AllHas
    , Has(..)
    , Project(..)
    ) where

import Data.Distinct.Catalog.Internal
