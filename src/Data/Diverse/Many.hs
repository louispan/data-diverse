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
    , nul
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
    , fetchN
    -- ** Setter for single field
    , replace
    , replace'
    , replaceN
    , replaceN'
    -- ** Lens for a single field
    , item
    , item'
    , itemN
    , itemN'

    -- * Multiple fields
    -- ** Getter for multiple fields
    , Select
    , select
    , SelectN
    , selectN
    -- ** Setter for multiple fields
    , Amend
    , amend
    , Amend'
    , amend'
    , AmendN
    , amendN
    , AmendN'
    , amendN'
    -- ** Lens for multiple fields
    , project
    , project'
    , projectN
    , projectN'

    -- * Destruction
    -- ** By type
    , Via -- no constructor
    , via -- safe construction
    , forMany
    , collect
    -- ** By Nat index offset
    , ViaN -- no constructor
    , viaN -- safe construction
    , forManyN
    , collectN
    ) where

import Data.Diverse.Many.Internal
