-- | Re-export Nary without the constructor
module Data.Diverse.Nary
    ( Nary -- ^ Hide constructor
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
    , forNary
    , collect
    , Narrow
    , narrow
    , (\^.)
    , Amend
    , amend
    , (\.~)
    , project
    , projected
    ) where

import Data.Diverse.Nary.Internal
