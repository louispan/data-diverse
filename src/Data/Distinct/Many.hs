-- | Re-export Many without the constructor
module Data.Distinct.Many
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
    , Switch(..)
    , Case(..)
    , foldMany
    , Cases(..)
    , cases
    , CaseTypeable(..)
    ) where

import Data.Distinct.Many.Internal
