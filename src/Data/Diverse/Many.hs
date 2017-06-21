-- | Re-export Many without the constructor
module Data.Diverse.Many (
      -- * 'Many' type
      Many -- hiding constructor

      -- * Single type
      -- ** Construction
    , pick
    , pick'
    , pickN
      -- ** Destruction
    , notMany
    , trial
    , trial'
    , trialN
      -- ** Lens
    , facet
    , facetN

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    , reinterpret
    , ReinterpretN
    , reinterpretN'
      -- ** Lens
    , inject
    , injectN

      -- * Catamorphism
    , Switch(..)
    , forMany
    , switch
    , SwitchN(..)
    , forManyN
    , switchN
    ) where

import Data.Diverse.Many.Internal
