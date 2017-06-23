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
    , (.^.)
    , fetchN
    -- ** Setter for single field
    , replace
    , (.~.)
    , replaceN
    -- ** Lens for a single field
    , item
    , itemN

    -- * Multiple fields
    -- ** Getter for multiple fields
    , Narrow
    , narrow
    , (\^.)
    , NarrowN
    , narrowN
    -- ** Setter for multiple fields
    , Amend
    , amend
    , (\~.)
    , AmendN
    , amendN
    -- ** Lens for multiple fields
    , project
    , projectN

    -- * Destruction
    -- ** By type
    , Via -- no constructor
    , via -- safe construction
    , forMany
    , collect
    -- * By Nat index offset
    , ViaN -- no constructor
    , viaN -- safe construction
    , forManyN
    , collectN
    ) where

import Data.Diverse.Many.Internal
