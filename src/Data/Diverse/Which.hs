-- | Re-export Which without the constructor
module Data.Diverse.Which (
      -- * 'Which' type
      Which -- hiding constructor

      -- * Single type
      -- ** Construction
    , zilch
    , pick
    , pick0
    , pickOnly
    , pickL
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial'
    , trial0
    , trial0'
    , trialL
    , trialL'
    , trialN
    , trialN'

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , diversify0
    , diversifyL
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    , reinterpret
    , Reinterpret'
    , reinterpret'
    , reinterpretL
    , reinterpretL'
    , ReinterpretN'
    , reinterpretN'

      -- * Catamorphism
    , Switch
    , Switcher(..)
    , which
    , switch
    , SwitchN
    , SwitcherN(..)
    , whichN
    , switchN
    ) where

import Data.Diverse.Which.Internal
