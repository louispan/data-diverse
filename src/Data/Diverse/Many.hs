-- | Re-export Many without the constructor
module Data.Diverse.Many (
    -- * 'Many' type
      Many -- Hiding constructor

      -- * Isomorphism
    , IsMany(..)
    , fromMany'
    , toMany'
    , _Many
    , _Many'

      -- * Construction
    , nil
    , single
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)

    -- * Simple queries
    , front
    , back
    , aft
    , fore

    -- * Single field
    -- ** Getter for single field
    , fetch
    , fetchL
    , fetchN
    -- ** Setter for single field
    , replace
    , replace'
    , replaceL
    , replaceL'
    , replaceN
    , replaceN'
    -- ** Lens for a single field
    , item
    , item'
    , itemL
    , itemL'
    , itemN
    , itemN'

    -- * Multiple fields
    -- ** Getter for multiple fields
    , Select
    , select
    , selectL
    , SelectN
    , selectN
    -- ** Setter for multiple fields
    , Amend
    , amend
    , Amend'
    , amend'
    , amendL
    , amendL'
    , AmendN
    , amendN
    , AmendN'
    , amendN'
    -- ** Lens for multiple fields
    , project
    , project'
    , projectL
    , projectL'
    , projectN
    , projectN'

    -- * Destruction
    -- ** By type
    , forMany
    , collect
    -- ** By Nat index offset
    , forManyN
    , collectN
    ) where

import Data.Diverse.Many.Internal
