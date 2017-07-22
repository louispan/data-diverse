-- | Re-export Many without the constructor
module Data.Diverse.Many (
    -- * 'Many' type
      Many -- Hiding constructor

      -- * Isomorphism
    , IsMany(..)
    , fromMany'
    , toMany'

      -- * Construction
    , nil
    , single
    , prefix
    , (./)
    , postfix
    , postfix'
    , (\.)
    , append
    , append'
    , (/./)

    -- * Simple queries
    , viewf
    , viewb
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

    -- * Destruction
    -- ** By type
    , forMany
    , collect
    -- ** By Nat index offset
    , forManyN
    , collectN
    ) where

import Data.Diverse.Many.Internal
