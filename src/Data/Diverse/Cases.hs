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

module Data.Diverse.Cases where

import Data.Diverse.Case
import Data.Diverse.Nary
import Data.Diverse.Reiterate
import Data.Diverse.Type
import Data.Kind

-- | Contains a 'Nary' of handlers/continuations for all the types in the 'xs' typelist.
newtype Cases (fs :: [Type]) (xs :: [Type]) r = Cases (Nary fs)

instance Reiterate (Cases fs) xs where
    reiterate (Cases s) = Cases s

-- | UndecidableIstnaces because fs appers more often.
instance (Distinct fs, Member (Head xs -> r) fs) => Case (Cases fs) xs r where
    then' (Cases s) = fetch @(Head xs -> r) s

-- | Create Cases for handling 'switch' from a tuple.
-- This function imposes additional constraints than using 'Cases' constructor directly:
-- * SameLength constraints to prevent human confusion with unusable cases.
-- * OutcomeOf fs ~ r constraints to ensure that the Nary only continutations that return r.
-- Example: @switch a $ cases (f, g, h)@
cases :: (SameLength fs xs, OutcomeOf fs ~ r) => Nary fs -> (Cases fs) xs r
cases = Cases
