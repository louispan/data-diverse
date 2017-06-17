-- | Re-export Nary without the constructor
module Data.Diverse.Nary (
    -- * 'Nary' type
      Nary -- Hiding constructor
    -- * Construction
    , blank
    , (.|)
    , singleton
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)
    -- * Query
    , front
    , back
    , aft
    , fore
    -- * Single field
    -- ** Getter for single field
    , fetch
    , (.^.)
    -- ** Setter for single field
    , replace
    , (..~)
    -- ** Lens
    , item
    -- * Multiple fields
    -- ** Getter for multiple fields
    , Narrow
    , narrow
    , (\^.)
    -- ** Setter for multiple fields
    , Amend
    , amend
    , (\.~)
    -- ** Lens for multiple fields
    , project
    , projected
    -- * Catamorphism
    , Via -- no constructor
    , via -- safe construction
    , forNary
    , collect
    ) where

import Data.Diverse.Nary.Internal
