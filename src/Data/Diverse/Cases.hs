{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Cases
    ( Cases(..)
    , cases
    , CasesN
    , casesN
    ) where

import Data.Diverse.Case
import Data.Diverse.Many
import Data.Diverse.Reiterate
import Data.Diverse.Type
import Data.Kind
import Data.Proxy
import GHC.TypeLits

-- | Contains a 'Many' of handlers/continuations for all the types in the 'xs' typelist.
-- This uses 'fetch' to get the handler for the type of the field found.
newtype Cases (fs :: [Type]) (xs :: [Type]) r = Cases (Many fs)

instance Reiterate (Cases fs) xs where
    reiterate (Cases s) = Cases s

-- | UndecidableIstnaces because fs appers more often.
instance UniqueMember (Head xs -> r) fs => Case (Cases fs) xs r where
    case' (Cases s) = fetch @(Head xs -> r) s

-- | Create and instance of 'Case' for handling 'switch' from a 'Many'.
--
-- Example: @switch a $ cases (f, g, h)@
--
-- This function imposes additional constraints than using 'Cases' constructor directly:
--
-- * @SameLength@ constraints to prevent human confusion with unused/unusable cases.
-- * @OutcomeOf fs ~ r@ constraints to ensure that the 'Many' only continutations that return r.
cases :: (SameLength fs (Distinct xs), OutcomeOf fs ~ r) => Many fs -> (Cases fs) xs r
cases = Cases

-----------------------------------------------

-- | Contains a 'Many' of handlers/continuations for all the types in the 'xs' typelist.
-- This version uses 'fetchN' to get the handler, and therefore there may be different
-- handlers for the same type, but the handlers must be in the same order has the input Many
newtype CasesN (fs :: [Type]) (n :: Nat) (xs :: [Type]) r = CasesN (Many fs)

instance ReiterateN (CasesN fs) n xs where
    reiterateN (CasesN s) = CasesN s

-- | UndecidableIstnaces because fs appers more often.
instance (MemberAt n (Head xs -> r) fs) => Case (CasesN fs n) xs r where
    case' (CasesN s) = fetchN (Proxy @n) s

-- | Safe Constructor for CasesN ensuring that the n Nat starts at 0
casesN :: OutcomeOf fs ~ r => Many fs -> CasesN fs 0 xs r
casesN = CasesN
