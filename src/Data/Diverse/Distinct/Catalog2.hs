-- | Re-export Catalog without the constructor
module Data.Diverse.Distinct.Catalog2
    ( Catalog -- ^ Hide constructor
    , blank
    , (.|)
    , singleton
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)
    , front
    , back
    , aft
    , fore
    , fetch
    , (.^.)
    , replace
    , (..~)
    , item
    , Via -- ^ no constructor
    , via -- ^ safe construction
    , forCatalog
    , collect
    , Cases(..)
    , Narrow
    , narrow
    , (\^.)
    , Amend
    , amend
    , (\.~)
    , project
    , projected
    ) where

import Data.Diverse.Distinct.Catalog2.Internal
