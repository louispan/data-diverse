-- | Re-export Which without the constructor
module Data.Diverse.Which (
      -- * 'Which' type
      Which -- hiding constructor

      -- * Single type
      -- ** Construction
    , impossible
    , pick
    , pick'
    , pickN
      -- ** Destruction
    , conclude
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
    , which
    , switch
    , SwitchN(..)
    , whichN
    , switchN
    ) where

import Data.Diverse.Which.Internal
