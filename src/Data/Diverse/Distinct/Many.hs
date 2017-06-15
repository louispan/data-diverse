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
    , diversify
    , reinterpret
    , reinterpretEither
    , inject
    , injected
    , _many
    , Switch
    , switch
    , Cases(..)
    , cases
    , caseTypeable
    ) where

import Data.Diverse.Distinct.Many.Internal
