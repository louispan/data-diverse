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
