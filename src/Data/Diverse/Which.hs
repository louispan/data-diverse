-- | Re-export Which without the constructor
module Data.Diverse.Which (
      -- * 'Which' type
      Which -- hiding constructor

      -- * Single type
      -- ** Construction
    , impossible
    , pick
    , pick0
    , pickOnly
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial0
    , trialN
      -- ** Lens
    , facet
    , facetN

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , diversify0
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    , reinterpret
    , ReinterpretN
    , reinterpretN
      -- ** Lens
    , inject
    , injectN

      -- * Catamorphism
    , Switch(..)
    , which
    , switch
    , SwitchN(..)
    , whichN
    , switchN
    ) where

import Data.Diverse.Which.Internal
