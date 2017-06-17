-- | Re-export Many without the constructor
module Data.Diverse.Many (
      -- * 'Many' type
      Many -- hiding constructor
      -- * Single type
      -- ** Construction
    , pick
    , pick'
      -- ** Destruction
    , notMany
    , trial
    , trial'
    , trialEither
    , trialEither'
      -- ** Lens
    , facet
      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , Reinterpret
      -- ** Inverse Injection
    , reinterpret
    , reinterpretEither
      -- ** Lens
    , inject
    , injected
      -- * Catamorphism
    , Switch(..)
    , forMany
    , switch
    ) where

import Data.Diverse.Many.Internal
