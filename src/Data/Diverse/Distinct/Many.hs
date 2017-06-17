-- | Re-export Many without the constructor
module Data.Diverse.Distinct.Many
    ( Many -- ^ Hide constructor
    , pick
    , pick'
    , notMany
    , trial
    , trial'
    , trialEither
    , trialEither'
    , facet
    , Diversify
    , diversify
    , Reinterpret
    , reinterpret
    , reinterpretEither
    , inject
    , injected
    , forMany
    , Switch(..)
    , switch
    , Cases(..)
    , cases
    ) where

import Data.Diverse.Distinct.Many.Internal
