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
    , Case(..)
    , foldMany
    , Switch(..)
    , Cases(..)
    , cases
    , CaseTypeable(..)
    ) where

import Data.Diverse.Distinct.Many.Internal
