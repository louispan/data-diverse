{-# LANGUAGE ConstraintKinds #-}
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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Diverse.Cases
    ( Cases
    , cases
    , cases'
    , CasesN
    , casesN
    , casesN'
    ) where

import Data.Diverse.Case
import Data.Diverse.Many
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind
import GHC.TypeLits

-- | Contains a 'Many' of handlers/continuations for all the types in the 'xs' typelist.
-- This uses __'grab'__ to get the unique handler for the type at the 'Head' of @xs@.
--
-- Use 'cases' to construct this with 'SameLength' constraint to reduce programming confusion.
newtype Cases (fs :: [Type]) r (xs :: [Type]) = Cases (Many fs)

type instance CaseResult (Cases fs r) x = r

instance Reiterate (Cases fs r) xs where
    reiterate (Cases s) = Cases s

-- | UndecidableInstances because @fs@ appears more often.
instance UniqueMember (Head xs -> r) fs => Case (Cases fs r) xs where
    case' (Cases s) = grab @(Head xs -> r) s

-- | Create an instance of 'Case' for either handling 'Data.Diverse.Which.switch'ing a 'Which'.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y (
--     'cases' (show \@Bool
--         './' show \@Int
--         './' 'nul')) \`shouldBe` "5"
-- @
--
-- Or for handling 'collect' from a 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Char './' show \@(Maybe Char) './' show \@Bool './' 'nul'
-- 'Data.Diverse.AFoldable.afoldr' (:) [] ('collect' x ('cases' y)) \`shouldBe`
--     [\"5", \"False", \"'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
--
-- This function imposes additional @SameLength@ constraints than when using the 'Cases' constructor directly.
-- It is better practice to use 'cases' to prevent programming confusion with dead code.
-- However, the 'Cases' constructor is still exported to allow creating a master-of-all-'Case'.
cases
    :: forall r xs fs.
       (AllConstrained ((~) r) (CaseResults (Cases fs r) fs), SameLength fs (Nub xs))
    => Many fs -> Cases fs r xs
cases = Cases

-- | A variation of 'cases' without the @SameLength@ constraint to allow creating a master-of-all-'Case'.
cases'
    :: forall r xs fs.
       (AllConstrained ((~) r) (CaseResults (Cases fs r) fs))
    => Many fs -> Cases fs r xs
cases' = Cases

-----------------------------------------------

-- | A variation of 'Cases' which uses __'grabN'__ to get the handler by index.
-- There may be different handlers for the same type, but the handlers must be in the same order
-- as the input @xs@ typelist.
-- Use 'casesN' to construct this safely ensuring @n@ starts at 0.
newtype CasesN (fs :: [Type]) r (n :: Nat) (xs :: [Type]) = CasesN (Many fs)

type instance CaseResult (CasesN fs r n) x = r

instance ReiterateN (CasesN fs r) n xs where
    reiterateN (CasesN s) = CasesN s

-- | UndecidableInstances because @fs@ appears more often.
instance (MemberAt n (Head xs -> r) fs) => Case (CasesN fs r n) xs where
    case' (CasesN s) = grabN @n s

-- | Safe Constructor for 'CasesN' ensuring that the @n@ Nat starts at 0.
-- It is an instance of 'CaseN' for either handling 'Data.Diverse.Which.switchN'ing a 'Which' in index order.
--
-- @
-- let y = 'Data.Diverse.Which.pickN' @0 Proxy (5 :: Int) :: Which '[Int, Bool, Bool, Int]
-- 'Data.Diverse.Which.switchN' y (
--     'casesN' (show \@Int
--         './' show \@Bool
--         './' show \@Bool
--         './' show \@Int
--         './' 'nul')) \`shouldBe` "5"
-- @
--
-- Or for handling 'collectN' from a 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Bool './' show \@Char './' show \@(Maybe Char) './' show \@Int './' show \@(Maybe Char) './' 'nul'
-- 'Data.Diverse.AFoldable.afoldr' (:) [] ('collectN' x ('casesN' y)) \`shouldBe`
--     [\"5", \"False", \"'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
casesN
    :: forall r xs fs.
       (AllConstrained ((~) r) (CaseResults (CasesN fs r 0) fs), SameLength fs xs)
    => Many fs -> CasesN fs r 0 xs
casesN = CasesN

-- | A variation of 'casesN' without the @SameLength@ constraint to allow creating a master-of-all-'Case'.
casesN'
    :: forall r xs fs.
       (AllConstrained ((~) r) (CaseResults (CasesN fs r 0) fs))
    => Many fs -> CasesN fs r 0 xs
casesN' = CasesN
