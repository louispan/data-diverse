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
    , Facet(..)
    , diversify
    , reinterpret
    , reinterpretEither
    , Inject(..)
    , Switch(..)
    , Case(..)
    , foldMany
    , Cases(..)
    , cases
    , CaseTypeable(..)
    ) where

import Data.Distinct.Many.Internal
