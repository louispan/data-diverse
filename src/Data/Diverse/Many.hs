-- | Re-export Many without the constructor
module Data.Diverse.Many
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
    ) where

import Data.Diverse.Many.Internal
