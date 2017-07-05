{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Which.Internal (
      -- * 'Which' type
      Which(..) -- exporting constructor unsafely!

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
      -- ** Lens
    , facet
    , facetL
    , facetN

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
      -- ** Lens
    , inject
    , injectL
    , injectN

      -- * Catamorphism
    , Switch(..)
    , which
    , switch
    , SwitchN(..)
    , whichN
    , switchN
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Diverse.Case
import Data.Diverse.Reduce
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind
import Data.Proxy
import qualified GHC.Generics as G
import GHC.Prim (Any, coerce)
import GHC.TypeLits
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L
import Unsafe.Coerce

-- | A 'Which' is an anonymous sum type (also known as a polymorphic variant, or co-record)
-- which can only contain one of the types in the typelist.
-- This is essentially a typed version of 'Data.Dynamic'.
--
-- The following functions are available can be used to manipulate unique types in the typelist
--
-- * constructor: 'pick'
-- * destructor: 'trial'
-- * injection: 'diversify' and 'reinterpret'
-- * catamorphism: 'which' or 'switch'
--
-- These functions are type specified. This means labels are not required because the types themselves can be used to access the 'Which'.
-- It is a compile error to use those functions for duplicate fields.
--
-- For duplicate types in the list of possible types, Nat-indexed version of the functions are available:
--
-- * constructor: 'pickN'
-- * destructor: 'trialN'
-- * inejction: 'diversifyN' and 'reinterpretN'
-- * catamorphism: 'whichN' or 'switchN'
--
-- Encoding: The variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as <https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Variant.hs Haskus.Util.Variant> and <https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Variant.html Data.Hlist.Variant>.
--
-- The constructor is only exported in the "Data.Diverse.Which.Internal" module
data Which (xs :: [Type]) = Which {-# UNPACK #-} !Int Any

-- Just like Haskus and HList versions, inferred type is phantom which is wrong
-- representational means:
-- @
-- Coercible '[Int] '[IntLike] => Coercible (Which '[Int]) (Which '[IntLike])
-- @
type role Which representational

----------------------------------------------

-- | A terminating 'G.Generic' instance for no types encoded as a 'impossible'.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which '[]) where
  type Rep (Which '[]) = G.U1
  from _ = {- G.U1 -} G.U1
  to G.U1 = impossible

-- | A terminating 'G.Generic' instance for one type encoded with 'pick''.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which '[x]) where
    type Rep (Which '[x]) = G.Rec0 x
    from v = {- G.Rec0 -} G.K1 (obvious v)
    to ({- G.Rec0 -} G.K1 a) = pickOnly a

-- | A 'G.Generic' instance encoded as either the 'x' value ('G.:+:') or the 'diversify0'ed remaining 'Which xs'.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which (x ': x' ': xs)) where
    type Rep (Which (x ': x' ': xs)) = (G.Rec0 x) G.:+: (G.Rec0 (Which (x' ': xs)))
    from v = case trial0 v of
            Right x -> G.L1 ({- G.Rec0 -} G.K1 x)
            Left v' -> G.R1 ({- G.Rec0 -} G.K1 v')
    to {- G.Rec0 -} x = case x of
        G.L1 ({- G.Rec0 -} G.K1 a) -> pick0 a
        G.R1 ({- G.Rec0 -} G.K1 v) -> diversify0 v

-----------------------------------------------------------------------

-- | A 'Which' with no alternatives. You can't do anything with 'impossible'
-- except Eq, Read, and Show it.
-- Using functions like 'switch' and 'trial' with 'impossible' is a compile error.
-- 'impossible' is only useful as a 'Left'-over from 'trial'ing a @Which '[x]@ with one type.
impossible :: Which '[]
impossible = Which (-1) (unsafeCoerce ())

-- | Lift a value into a 'Which' of possibly other types @xs@.
-- @xs@ can be inferred or specified with TypeApplications.
-- NB. forall is used to specify @xs@ first, so TypeApplications can be used to specify @xs@ first
--
-- @
-- 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: Which '[Int, Bool, Char, Maybe String]
-- @
pick :: forall xs x. UniqueMember x xs => x -> Which xs
pick = pick_

pick_ :: forall x xs n. (KnownNat n, n ~ IndexOf x xs) => x -> Which xs
pick_ = Which (fromInteger (natVal @n Proxy)) . unsafeCoerce

-- | A variation of 'pick' where @x@ is specified via a label
--
-- @
-- let y = 'pickL' \@Foo Proxy (Tagged (5 :: Int)) :: Which '[Bool, Tagged Foo Int, Tagged Bar Char]
--     x = 'trialL' \@Foo Proxy y
-- x `shouldBe` (Right (Tagged 5))
-- @
pickL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> x -> Which xs
pickL _ = pick_ @x

-- | A variation of 'pick' into a 'Which' of a single type.
--
-- @
-- 'pickOnly' \'A' :: Which '[Char]
-- @
pickOnly :: x -> Which '[x]
pickOnly = pick0

-- | A variation of 'pick' into a 'Which' where @x@ is the first type.
--
-- @
-- 'pick0' \'A' :: Which '[Char, Int, Bool]
-- @
pick0 :: x -> Which (x ': xs)
pick0 = Which 0 . unsafeCoerce

-- | Lift a value into a 'Which' of possibly other (possibley indistinct) types, where the value is the @n@-th type.
--
-- @
-- 'pickN' (Proxy \@4) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
-- @
pickN :: forall n xs x proxy. MemberAt n x xs => proxy n -> x -> Which xs
pickN _ = Which (fromInteger (natVal @n Proxy)) . unsafeCoerce

-- | It is 'obvious' what value is inside a 'Which' of one type.
--
-- @
-- let x = 'pick'' \'A' :: Which '[Char]
-- 'obvious' x \`shouldBe` \'A'
-- @
obvious :: Which '[a] -> a
obvious (Which _ v) = unsafeCoerce v

-- | 'trial' a type in a 'Which' and 'Either' get the 'Right' value or the 'Left'-over possibilities.
--
-- @
-- let x = 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trial' \@Char x \`shouldBe` Right \'A'
-- 'trial' \@Int x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Bool, Char, Maybe String]
-- @
trial
    :: forall x xs.
       (UniqueMember x xs)
    => Which xs -> Either (Which (Without x xs)) x
trial = trial_

trial_
    :: forall x xs n.
       (KnownNat n, n ~ IndexOf x xs)
    => Which xs -> Either (Which (Without x xs)) x
trial_ (Which n v) = let i = fromInteger (natVal @n Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Which (n - 1) v)
                          else Left (Which n v)

-- | A variation of 'trial' where x is specified via a label
--
-- @
-- let y = 'pickL' \@Foo Proxy (Tagged (5 :: Int)) :: Which '[Bool, Tagged Foo Int, Tagged Bar Char]
--     x = 'trialL' \@Foo Proxy y
-- x `shouldBe` (Right (Tagged 5))
-- @
trialL
    :: forall l xs x proxy.
       (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
    => proxy l -> Which xs -> Either (Which (Without x xs)) x
trialL _ = trial_ @x

-- | A variation of a 'Which' 'trial' which 'trial's the first type in the type list.
--
-- @
-- let x = 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trial0' x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Bool, Char, Maybe String]
-- @
trial0 :: Which (x ': xs) -> Either (Which xs) x
trial0 (Which n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Which (n - 1) v)

-- | 'trialN' the n-th type of a 'Which', and get 'Either' the 'Right' value or the 'Left'-over possibilities.
--
-- @
-- let x = 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trialN' @1 Proxy x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Int, Char, Maybe String]
-- @
trialN
    :: forall n xs x proxy.
       (MemberAt n x xs)
    => proxy n -> Which xs -> Either (Which (WithoutIndex n xs)) x
trialN _ (Which n v) = let i = fromInteger (natVal @n Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Which (n - 1) v)
                          else Left (Which n v)

-- | Utility to convert Either to Maybe
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-----------------------------------------------------------------

-- | 'pick' ('review' 'facet') and 'trial' ('preview' 'facet') in 'Prism'' form.
--
-- @
-- 'facet' = 'prism'' 'pick' (either (const Nothing) Just . 'trial')
-- @
--
-- @
-- let y = 'review' ('facet' \@Int) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Char] -- 'pick'
--     x = 'preview' ('facet' \@Int) y -- 'trial'
-- x \`shouldBe` (Just 5)
-- @
facet :: forall x xs. (UniqueMember x xs) => Prism' (Which xs) x
facet = prism' pick (hush . trial)
{-# INLINE facet #-}

-- | 'pickL' ('review' 'facetL') and 'trialL' ('preview' 'facetL') in 'Prism'' form.
--
-- @
-- let y = 'review' ('facetL' \@Bar Proxy) (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
--     x = 'preview' ('facetL' \@Bar Proxy) y
-- x \`shouldBe` (Just (Tagged 5))
-- @
facetL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Prism' (Which xs) x
facetL p = prism' (pickL p) (hush . trialL p)
{-# INLINE facetL #-}

-- | 'pickN' ('review' 'facetN') and 'trialN' ('preview' 'facetN') in 'Prism'' form.
--
-- @
-- 'facetN' p = 'prism'' ('pickN' p) (either (const Nothing) Just . 'trialN' p)
-- @
--
-- @
-- let y = 'review' ('facetN' (Proxy \@4)) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Int, Char] -- 'pickN'
--     x = 'preview' ('facetN' (Proxy \@4)) y -- 'trialN'
-- x \`shouldBe` (Just 5)
-- @
facetN :: forall n xs x proxy. (MemberAt n x xs) => proxy n -> Prism' (Which xs) x
facetN p = prism' (pickN p) (hush . trialN p)
{-# INLINE facetN #-}

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'diversify'.
type Diversify (tree :: [Type]) (branch :: [Type]) = Reduce (Which branch) (Switch (CaseDiversify tree branch) branch) (Which tree)

-- | Convert a 'Which' to another 'Which' that may include other possibilities.
-- That is, @branch@ is equal or is a subset of @tree@.
--
-- This can also be used to rearrange the order of the types in the 'Which'.
--
-- It is a compile error if @tree@ has duplicate types with @branch@.
--
-- NB. forall is used to @tree@ is ordered first, so TypeApplications can be used to specify @tree@ first.
--
-- @
-- let a = 'pick'' (5 :: Int) :: 'Which' '[Int]
--     b = 'diversify' \@[Int, Bool] a :: 'Which' '[Int, Bool]
--     c = 'diversify' \@[Bool, Int] b :: 'Which' '[Bool, Int]
-- @
diversify :: forall tree branch. Diversify tree branch => Which branch -> Which tree
diversify = which (CaseDiversify @tree @branch @branch)

data CaseDiversify (tree :: [Type]) (branch :: [Type]) (branch' :: [Type]) r = CaseDiversify

instance Reiterate (CaseDiversify tree branch) branch' where
    reiterate CaseDiversify = CaseDiversify

-- | The @Unique x branch@ is important to get a compile error if the from @branch@ doesn't have a unique x
instance (UniqueMember x tree, Unique x branch) =>
         Case (CaseDiversify tree branch) (x ': branch') (Which tree) where
    case' CaseDiversify = pick

-- | A simple version of 'diversify' which add another type to the front of the typelist.
diversify0 :: forall x xs. Which xs -> Which (x ': xs)
diversify0 (Which n v) = Which (n + 1) v

------------------------------------------------------------------

-- | A variation of 'diversify' where @branch@is additionally specified by a labels list.
--
-- @
-- let y = 'pickOnly' (5 :: Tagged Bar Int)
--     y' = 'diversifyL' \@'[Bar] Proxy y :: 'Which' '[Tagged Bar Int, Tagged Foo Bool]
--     y'' = 'diversifyL' \@'[Bar, Foo] Proxy y' :: 'Which' '[Tagged Foo Bool, Tagged Bar Int]
-- 'switch' y'' ('Data.Diverse.CaseTypeable.CaseTypeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` \"Tagged * Bar Int"
-- @
diversifyL
    :: forall ls tree branch proxy.
       ( Diversify tree branch
       , branch ~ KindsAtLabels ls tree
       , UniqueLabels ls tree
       , IsDistinct ls
       )
    => proxy ls -> Which branch -> Which tree
diversifyL _ = which (CaseDiversify @tree @branch @branch)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'diversifyN'.
type DiversifyN (indices :: [Nat]) (tree :: [Type]) (branch :: [Type]) =
    ( Reduce (Which branch) (SwitchN (CaseDiversifyN indices) 0 branch) (Which tree)
    , KindsAtIndices indices tree ~ branch)

-- | A variation of 'diversify' which uses a Nat list @indices@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx
-- @
--
-- This variation allows @tree@ to contain duplicate types with @branch@ since
-- the mapping is specified by @indicies@.
--
-- @
-- let y = 'pickOnly' (5 :: Int)
--     y' = 'diversifyN' \@'[0] \@[Int, Bool] Proxy y
--     y'' = 'diversifyN' \@[1,0] \@[Bool, Int] Proxy y'
-- 'switch' y'' ('Data.Diverse.CaseTypeable.CaseTypeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` \"Int"
-- @
diversifyN :: forall indices tree branch proxy. (DiversifyN indices tree branch) => proxy indices -> Which branch -> Which tree
diversifyN _ = whichN (CaseDiversifyN @indices @0 @branch)

data CaseDiversifyN (indices :: [Nat]) (n :: Nat) (branch' :: [Type]) r = CaseDiversifyN

instance ReiterateN (CaseDiversifyN indices) n branch' where
    reiterateN CaseDiversifyN = CaseDiversifyN

instance MemberAt (KindAtIndex n indices) x tree =>
         Case (CaseDiversifyN indices n) (x ': branch') (Which tree) where
    case' CaseDiversifyN v = pickN (Proxy @(KindAtIndex n indices)) v

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpret'.
type Reinterpret branch tree = Reduce (Which tree) (Switch (CaseReinterpret branch tree) tree) (Either (Which (Complement tree branch)) (Which branch))

-- | Convert a 'Which' into possibly another 'Which' with a totally different typelist.
-- Returns either a 'Which' with the 'Right' value, or a 'Which' with the 'Left'over @compliment@ types.
--
-- It is a compile error if @branch@ or @compliment@ has duplicate types with @tree@.
--
-- NB. forall used to specify @branch@ first, so TypeApplications can be used to specify @branch@ first.
--
-- @
--     let a = 'pick' \@[Int, Char, Bool] (5 :: Int) :: 'Which' '[Int, Char, Bool]
--     let  b = 'reinterpret' @[String, Char] y
--     b \`shouldBe` Left ('pick' (5 :: Int)) :: 'Which' '[Int, Bool]
--     let c = 'reinterpret' @[String, Int] a
--     c \`shouldBe` Right ('pick' (5 :: Int)) :: 'Which' '[String, Int]
-- @
reinterpret :: forall branch tree. Reinterpret branch tree => Which tree -> Either (Which (Complement tree branch)) (Which branch)
reinterpret = which (CaseReinterpret @branch @tree @tree)

data CaseReinterpret (branch :: [Type]) (tree :: [Type]) (tree' :: [Type]) r = CaseReinterpret

instance Reiterate (CaseReinterpret branch tree) tree' where
    reiterate CaseReinterpret = CaseReinterpret

instance ( MaybeUniqueMemberAt n x branch
         , comp ~ Complement tree branch
         , MaybeUniqueMemberAt n' x comp
         , Unique x tree -- Compile error to ensure reinterpret only works with unique fields
         ) =>
         Case (CaseReinterpret branch tree) (x ': tree') (Either (Which comp) (Which branch)) where
    case' CaseReinterpret a =
        case fromInteger (natVal @n Proxy) of
            0 -> let j = fromInteger (natVal @n' Proxy)
                 -- safe use of partial! j will never be zero due to check above
                 in Left $ Which (j - 1) (unsafeCoerce a)
            i -> Right $ Which (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | A variation of 'reinterpret' where the @branch@ is additionally specified with a labels list.
--
-- @
-- let y = 'pick' \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
--     y' = 'reinterpretL' \@[Foo, Bar] Proxy y
--     x = 'pick' \@[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
-- y' \`shouldBe` Right x
-- @
reinterpretL
    :: forall ls branch tree proxy.
       ( Reinterpret branch tree
       , branch ~ KindsAtLabels ls tree
       , UniqueLabels ls tree
       , IsDistinct ls
       )
    => proxy ls
    -> Which tree
    -> Either (Which (Complement tree branch)) (Which branch)
reinterpretL _ = which (CaseReinterpret @branch @tree @tree)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpretN'.
type ReinterpretN (indices :: [Nat]) (branch :: [Type]) (tree :: [Type]) =
    ( Reduce (Which tree) (SwitchN (CaseReinterpretN indices) 0 tree) (Maybe (Which branch))
    , KindsAtIndices indices tree ~ branch)

-- | A limited variation of 'reinterpret' which uses a Nat list @n@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx
-- @
--
-- This variation allows @tree@ to contain duplicate types with @branch@
-- since the mapping is specified by @indicies@.
--
-- However, unlike 'reinterpert', in this variation,
-- @branch@ must be a subset of @tree@ instead of any arbitrary Which.
-- Also it returns a Maybe instead of Either.
--
-- This is so that the same @indices@ can be used in 'narrowN'.
reinterpretN :: forall (indices :: [Nat]) branch tree proxy. (ReinterpretN indices branch tree) => proxy indices -> Which tree -> Maybe (Which branch)
reinterpretN _ = whichN (CaseReinterpretN @indices @0 @tree)

data CaseReinterpretN (indices :: [Nat]) (n :: Nat) (tree' :: [Type]) r = CaseReinterpretN

instance ReiterateN (CaseReinterpretN indices) n tree' where
    reiterateN CaseReinterpretN = CaseReinterpretN

instance (MaybeMemberAt n' x branch, n' ~ PositionOf n indices) => Case (CaseReinterpretN indices n) (x ': tree) (Maybe (Which branch)) where
    case' CaseReinterpretN a =
        case fromInteger (natVal @n' Proxy) of
            0 -> Nothing
            i -> Just $ Which (i - 1) (unsafeCoerce a)

-- ------------------------------------------------------------------

-- | 'diversify' ('review' 'inject') and 'reinterpret' ('preview' 'inject') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' ('inject' \@_ \@[Bool, Int, Char, String]) x -- 'diversify'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('inject' \@[String, Int]) y -- 'reinterpret'
-- y' \`shouldBe` Just (pick (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @
inject
    :: forall branch tree.
       ( Diversify tree branch
       , Reinterpret branch tree
       )
    => Prism' (Which tree) (Which branch)
inject = prism' diversify (hush . reinterpret)
{-# INLINE inject #-}


-- | 'diversifyL' ('review' 'injectL') and 'reinterpretL' ('preview' 'injectL') in 'Prism'' form.
--
-- @
-- let t = 'pick' \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
--     b = 'pick' \@'[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
--     t' = 'review' ('injectL' \@[Foo, Bar] \@_ \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] Proxy) b
--     b' = 'preview' ('injectL' \@[Foo, Bar] Proxy) t'
-- t \`shouldBe` t'
-- b' \`shouldBe` Just b
-- @
injectL
    :: forall ls branch tree proxy.
       ( Diversify tree branch
       , Reinterpret branch tree
       , branch ~ KindsAtLabels ls tree
       , UniqueLabels ls tree
       , IsDistinct ls
       )
    => proxy ls -> Prism' (Which tree) (Which branch)
injectL p = prism' (diversifyL p) (hush . reinterpretL p)
{-# INLINE injectL #-}

-- | 'diversifyN' ('review' 'injectN') and 'reinterpretN' ('preview' 'injectN') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' (injectN \@[3, 1] \@_ \@[Bool, Int, Char, String] Proxy) x -- 'diversifyN'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('injectN' @[3, 1] \@[String, Int] Proxy) y -- 'reinterpertN''
-- y' \`shouldBe` Just ('pick' (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @
injectN
    :: forall indices branch tree proxy.
       ( DiversifyN indices tree branch
       , ReinterpretN indices branch tree
       )
    => proxy indices -> Prism' (Which tree) (Which branch)
injectN p = prism' (diversifyN p) (reinterpretN p)
{-# INLINE injectN #-}

------------------------------------------------------------------

-- | 'Switch' is an instance of 'Reduce' for which __'reiterate'__s through the possibilities in a 'Which',
-- delegating handling to 'Case', ensuring termination when 'Which' only contains one type.
newtype Switch c (xs :: [Type]) r = Switch (c xs r)

-- | 'trial0' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterate'__
-- trying the next type in the type list.
instance (Case c (x ': x' ': xs) r, Reduce (Which (x' ': xs)) (Switch c (x' ': xs)) r, Reiterate c (x : x' : xs)) =>
         Reduce (Which (x ': x' ': xs)) (Switch c (x ': x' ': xs)) r where
    reduce (Switch c) v =
        case trial0 v of
            Right a -> case' c a
            Left v' -> reduce (Switch (reiterate c)) v'
    -- GHC compilation is SLOW if there is no pragma for recursive typeclass functions for different types
    -- Using INLINEABLE instead of NOLINE so that ghc 8.2.1 can optimize to single case statement
    -- See https://ghc.haskell.org/trac/ghc/ticket/12877
    {-# INLINEABLE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
-- You can't reduce 'impossible'
instance (Case c '[x] r) => Reduce (Which '[x]) (Switch c '[x]) r where
    reduce (Switch c) v = case obvious v of
            a -> case' c a

-- | Catamorphism for 'Which'. This is equivalent to @flip 'switch'@.
which :: Reduce (Which xs) (Switch case' xs) r => case' xs r -> Which xs -> r
which = reduce . Switch

-- | A switch/case statement for 'Which'. This is equivalent to @flip 'which'@
--
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Which' of functions to a variant of values.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y (
--     'Data.Diverse.Cases.cases' (show \@Bool
--         'Data.Diverse.Many../' show \@Int
--         'Data.Diverse.Many../' 'Data.Diverse.Many.nil')) \`shouldBe` "5"
-- @
--
-- Or 'Data.Diverse.CaseTypeable.CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y ('CaseTypeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` "Int"
-- @
--
-- Or you may use your own custom instance of 'Case'.
switch :: Reduce (Which xs) (Switch case' xs) r => Which xs -> case' xs r -> r
switch = flip which

------------------------------------------------------------------

-- | 'SwitchN' is a variation of 'Switch' which __'reiterateN'__s through the possibilities in a 'Which',
-- delegating work to 'CaseN', ensuring termination when 'Which' only contains one type.
newtype SwitchN c (n :: Nat) (xs :: [Type]) r = SwitchN (c n xs r)

-- | 'trial0' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterateN'__
-- trying the next type in the type list.
instance (Case (c n) (x ': x' ': xs) r, Reduce (Which (x' ': xs)) (SwitchN c (n + 1) (x' ': xs)) r, ReiterateN c n (x : x' : xs)) =>
         Reduce (Which (x ': x' ': xs)) (SwitchN c n (x ': x' ': xs)) r where
    reduce (SwitchN c) v =
        case trial0 v of
            Right a -> case' c a
            Left v' -> reduce (SwitchN (reiterateN c)) v'
    -- GHC compilation is SLOW if there is no pragma for recursive typeclass functions for different types
    -- Using INLINEABLE instead of NOLINE so that ghc 8.2.1 can optimize to single case statement
    -- See https://ghc.haskell.org/trac/ghc/ticket/12877
    {-# INLINEABLE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
-- You can't reduce 'impossible'
instance (Case (c n) '[x] r) => Reduce (Which '[x]) (SwitchN c n '[x]) r where
    reduce (SwitchN c) v = case obvious v of
            a -> case' c a

-- | Catamorphism for 'Which'. This is equivalent to @flip 'switchN'@.
whichN :: Reduce (Which xs) (SwitchN case' n xs) r => case' n xs r -> Which xs -> r
whichN = reduce . SwitchN

-- | A switch/case statement for 'Which'. This is equivalent to @flip 'whichN'@
--
-- Use 'Case' instances like 'Data.Diverse.Cases.CasesN' to apply a 'Which' of functions to a variant of values
-- in index order.
--
-- @
-- let y = 'pickN' \@0 Proxy (5 :: Int) :: 'Which' '[Int, Bool, Bool, Int]
-- 'switchN' y (
--     'Data.Diverse.Cases.casesN' (show \@Int
--         'Data.Diverse.Many../' show \@Bool
--         'Data.Diverse.Many../' show \@Bool
--         'Data.Diverse.Many../' show \@Int
--         'Data.Diverse.Many../' 'Data.Diverse.Many.nil')) \`shouldBe` "5"
-- @
--
-- Or you may use your own custom instance of 'Case'.
switchN :: Reduce (Which xs) (SwitchN case' n xs) r => Which xs -> case' n xs r -> r
switchN = flip whichN

-----------------------------------------------------------------

-- | Two 'Which'es are only equal iff they both contain the equivalnet value at the same type index.
instance (Reduce (Which (x ': xs)) (Switch CaseEqWhich (x ': xs)) Bool) => Eq (Which (x ': xs)) where
    l@(Which i _) == (Which j u) =
        if i /= j
            then False
            else switch l (CaseEqWhich u)

-- | @('impossible' == 'impossible') == True@
instance Eq (Which '[]) where
    _ == _ = True

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseEqWhich (xs :: [Type]) r = CaseEqWhich Any

instance Reiterate CaseEqWhich (x ': xs) where
    reiterate (CaseEqWhich r) = CaseEqWhich r

instance (Eq x) => Case CaseEqWhich (x ': xs) Bool where
    case' (CaseEqWhich r) l = l == unsafeCoerce r

-----------------------------------------------------------------

-- | A 'Which' with a type at smaller type index is considered smaller.
instance ( Reduce (Which (x ': xs)) (Switch CaseEqWhich (x ': xs)) Bool
         , Reduce (Which (x ': xs)) (Switch CaseOrdWhich (x ': xs)) Ordering
         ) =>
         Ord (Which (x ': xs)) where
    compare l@(Which i _) (Which j u) =
        if i /= j
            then compare i j
            else switch l (CaseOrdWhich u)

-- | @('compare' 'impossible' 'impossible') == EQ@
instance Ord (Which '[]) where
    compare _ _ = EQ

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseOrdWhich (xs :: [Type]) r = CaseOrdWhich Any

instance Reiterate CaseOrdWhich (x ': xs) where
    reiterate (CaseOrdWhich r) = CaseOrdWhich r

instance (Ord x) => Case CaseOrdWhich (x ': xs) Ordering where
    case' (CaseOrdWhich r) l = compare l (unsafeCoerce r)

------------------------------------------------------------------

-- | @show ('pick'' \'A') == "pick \'A'"@
instance (Reduce (Which (x ': xs)) (Switch CaseShowWhich (x ': xs)) ShowS) => Show (Which (x ': xs)) where
    showsPrec d v = showParen (d > app_prec) (which (CaseShowWhich 0) v)
      where app_prec = 10

-- | @read "impossible" == 'impossible'@
instance Show (Which '[]) where
    showsPrec d _ = showParen (d > app_prec) (showString "impossible")
      where app_prec = 10

newtype CaseShowWhich (xs :: [Type]) r = CaseShowWhich Int

instance Reiterate CaseShowWhich (x ': xs) where
    reiterate (CaseShowWhich i) = CaseShowWhich (i + 1)

instance Show x => Case CaseShowWhich (x ': xs) ShowS where
    case' (CaseShowWhich i) v = showString "pickN @" . showString (show i) . showString " Proxy " . showsPrec (app_prec + 1) v
      where app_prec = 10

------------------------------------------------------------------

class WhichRead v where
    whichReadPrec :: Int -> Int -> ReadPrec v

data Which_ (xs ::[Type]) = Which_ Int Any

diversify0' :: forall x xs. Which_ xs -> Which_ (x ': xs)
diversify0' = coerce

readWhich_ :: forall x xs. Read x => Int -> Int -> ReadPrec (Which_ (x ': xs))
readWhich_ i j = guard (i == j) >> parens (prec app_prec $ (Which_ i . unsafeCoerce) <$> readPrec @x)
      where
        app_prec = 10

instance Read x => WhichRead (Which_ '[x]) where
    whichReadPrec = readWhich_

instance (Read x, WhichRead (Which_ (x' ': xs))) => WhichRead (Which_ (x ': x' ': xs)) where
    whichReadPrec i j = readWhich_ i j
               <|> (diversify0' <$> (whichReadPrec i (j + 1) :: ReadPrec (Which_ (x' ': xs))))
    -- GHC compilation is SLOW if there is no pragma for recursive typeclass functions for different types
    -- Using INLINEABLE instead of NOLINE so that ghc 8.2.1 can optimize to single case statement
    -- See https://ghc.haskell.org/trac/ghc/ticket/12877
    {-# INLINEABLE whichReadPrec #-}


-- | This 'Read' instance tries to read using the each type in the typelist, using the first successful type read.
instance WhichRead (Which_ (x ': xs)) =>
         Read (Which (x ': xs)) where
    readPrec =
        parens $ prec app_prec $ do
            lift $ L.expect (Ident "pickN")
            lift $ L.expect (Punc "@")
            i <- lift L.readDecP
            lift $ L.expect (Ident "Proxy")
            Which_ n v <- whichReadPrec i 0 :: ReadPrec (Which_ (x ': xs))
            pure $ Which n v
      where
        app_prec = 10

-- | @read "impossible" == 'impossible'@
instance Read (Which '[]) where
    readPrec =
        parens $ prec app_prec $ do
            lift $ L.expect (Ident "impossible")
            pure impossible
      where
        app_prec = 10
