-- | Re-export Catalog without the constructor
module Data.Distinct.Catalog
    ( Catalog
    -- , cataloged
    , _Cataloged
    , _Cataloged'
    , _Uncataloged
    , _Uncataloged'
    , Has(..)
    , Project(..)
    ) where

import Data.Distinct.Catalog.Internal
