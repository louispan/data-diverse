-- | Re-export Which without the constructor
module Data.Diverse.Which (
      -- * 'Which' type
      Which -- hiding constructor

      -- * Single type
      -- ** Construction
    , impossible
    , impossible'
    , pick
    , pick0
    , pickOnly
    , pickL
    , pickTag
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial'
    , trial0
    , trial0'
    , trialL
    , trialL'
    , trialTag
    , trialTag'
    , trialN
    , trialN'

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , diversify'
    , diversify0
    , DiversifyL
    , diversifyL
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    -- , Reinterpreted
    , reinterpret
    , Reinterpret'
    , reinterpret'
    , ReinterpretL
    -- , ReinterpretedL
    , reinterpretL
    , ReinterpretL'
    , reinterpretL'
    , ReinterpretN'
    , reinterpretN'

      -- * Catamorphism
    , Switch
    , switch
    , which
    , Switcher(..)
    , SwitchN
    , switchN
    , whichN
    , SwitcherN(..)
    ) where

import Data.Diverse.Which.Internal
