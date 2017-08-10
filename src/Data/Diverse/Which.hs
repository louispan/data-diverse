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
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial0
    , trialL
    , trialN

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
    , reinterpretL
    , ReinterpretN
    , reinterpretN

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
