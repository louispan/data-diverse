{-# LANGUAGE CPP #-}

-- | Re-export Many without the constructor
module Data.Diverse.Many (
      -- * 'Many' type
      -- | constructor not exported
      Many

      -- * Isomorphism
    , IsMany(..)
    , fromMany'
    , toMany'

      -- * Construction
    , nil
    , single
    , consMany
    , (./)
    , snocMany
    , snocMany'
    , (\.)
    , append
    -- , CanAppendUnique(..)
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
    , grab
    , grabL
    , grabTag
    , grabN
    -- ** Setter for single field
    , replace
    , replace'
    , replaceL
    , replaceL'
    , replaceTag
    , replaceTag'
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
    , Collect
    , Collector
    , forMany
    , collect
    -- ** By Nat index offset
    , CollectN
    , CollectorN
    , forManyN
    , collectN
    ) where

import Data.Diverse.Many.Internal
