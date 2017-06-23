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
    , pick'
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial'
    , trialN
      -- ** Lens
    , facet
    , facetN

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    , reinterpret
    , ReinterpretN
    , reinterpretN'
      -- ** Lens
    , inject
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
import Data.Diverse.AFoldable
import Data.Diverse.Case
import Data.Diverse.Collector
import Data.Diverse.Emit
import Data.Diverse.Reduce
import Data.Diverse.Reiterate
import Data.Diverse.Type
import Data.Kind
import Data.Proxy
import GHC.Prim (Any)
import GHC.TypeLits
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L
import Unsafe.Coerce

-- | A 'Which' is an anonymous sum type (also known as a polymorphic variant, or co-record)
-- which can only contain one of the types in the typelist.
-- This is essentially a typed version of 'Data.Dynamic'.
--
-- For unique types in the list of possible types, the types themselves become labels to allow easier to use constructor ('pick'), destructor ('trial'), injection ('diversify', 'reinterpret'), and catamorphism ('switch') using -XTypeApplications.
--
-- For duplicate types in the list of possible types, an indexed version of constructor ('pickN'), destructor ('trialN'), inejction ('diversifyN', 'reinterpretN''), and catamorphism ('switchN') is provided.
--
-- Encoding: The variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as <https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Variant.hs Haskus.Util.Variant> and <https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Variant.html Data.Hlist.Variant>.
--
-- The constructor is only exported in the "Data.Diverse.Which.Internal" module
data Which (xs :: [Type]) = Which {-# UNPACK #-} !Int Any

-- Just like Haskus and HList versions, inferred type is phantom which is wrong
type role Which representational

----------------------------------------------

-- | A 'Which' with no alternatives. You can't do anything with 'impossible'
-- except Eq, Read, and Show it.
-- Using functions like 'switch' and 'trial' with 'impossible' is a compile error.
-- 'impossible' is only useful as a 'Left'-over from 'trial'
impossible :: Which '[]
impossible = Which (-1) (unsafeCoerce ())

-- | Lift a value into a 'Which' of possibly other types @xs@ which can be inferred or specified with TypeApplications.
-- NB. forall is used to specify @xs@ first, so TypeApplications can be used to specify @xs@ first
--
-- @
-- 'pick' \'A' \@'[Int, Bool, Char, Maybe String]
-- @
pick :: forall xs x. UniqueMember x xs => x -> Which xs
pick = Which (fromInteger (natVal @(IndexOf x xs) Proxy)) . unsafeCoerce

-- | A variation of 'pick' into a 'Which' of a single type.
--
-- @
-- 'pick'' \'A'
-- @
pick' :: x -> Which '[x]
pick' = pick

-- | Lift a value into a 'Which' of possibly other (possibley indistinct) types, where the value is the @n@-th type.
--
-- @
-- 'pickN' (Proxy \@4) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
-- @
pickN :: forall n xs x proxy. MemberAt n x xs => proxy n -> x -> Which xs
pickN _ = Which (fromInteger (natVal @n Proxy)) . unsafeCoerce

-- | It is 'obvious' what value is inside a 'Which' of one type.
obvious :: Which '[a] -> a
obvious (Which _ v) = unsafeCoerce v

-- | 'trial' a confession out of a 'Which', and get 'Either' the 'Right' value or the 'Left'-over possibilities.
trial
    :: forall x xs.
       (UniqueMember x xs)
    => Which xs -> Either (Which (Without x xs)) x
trial (Which n v) = let i = fromInteger (natVal @(IndexOf x xs) Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Which (n - 1) v)
                          else Left (Which n v)

-- | A version of a 'Which' 'trial' which 'trial''s the first type in the type list.
trial' :: Which (x ': xs) -> Either (Which xs) x
trial' (Which n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Which (n - 1) v)


-- | 'trialN' the n-th confession out of a 'Which', and get 'Either' the 'Right' value or the 'Left'-over possibilities.
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

-- | A Which has a prism to an the inner type.
-- That is, a value can be 'pick'ed into a Which or mabye 'trial'ed out of a Which.
-- Use TypeApplication to specify the inner type of the of the prism.
-- Example: @facet \@Int@
facet :: forall x xs. (UniqueMember x xs) => Prism' (Which xs) x
facet = prism' pick (hush . trial)
{-# INLINE facet #-}

facetN :: forall n xs x proxy. (MemberAt n x xs) => proxy n -> Prism' (Which xs) x
facetN p = prism' (pickN p) (hush . trialN p)
{-# INLINE facetN #-}

------------------------------------------------------------------

-- | Convert a Which to another Which that may include other possibilities.
-- That is, @branch@ is equal or is a subset of @tree@.
-- This can be used to rearrange the order of the types in the Which.
-- NB. @tree@ is ordered first, so TypeApplications can be used to specify it.
diversify :: forall tree branch. Diversify tree branch => Which branch -> Which tree
diversify = which (CaseDiversify @tree @branch)

-- | A friendlier constraint synonym for 'diversify'. All 'Which' fufill this constraint.
type Diversify (tree :: [Type]) (branch :: [Type]) = (Reduce Which (Switch (CaseDiversify tree)) branch (Which tree), IsDistinct tree)

data CaseDiversify (tree :: [Type]) (branch' :: [Type]) r = CaseDiversify

instance Reiterate (CaseDiversify tree) branch' where
    reiterate CaseDiversify = CaseDiversify

-- | This uses unsafeToWhich - why?
instance (UniqueMember x tree) =>
         Case (CaseDiversify tree) (x ': branch') (Which tree) where
    case' CaseDiversify = pick

------------------------------------------------------------------

-- where indices[branch_idx] = tree_idx

diversifyN :: forall indices tree branch proxy. (DiversifyN indices tree branch) => proxy indices -> Which branch -> Which tree
diversifyN _ = whichN (CaseDiversifyN @indices @0 @branch)

type DiversifyN (indices :: [Nat]) (tree :: [Type]) (branch :: [Type]) = (Reduce Which (SwitchN (CaseDiversifyN indices) 0) (KindsAtIndices indices tree) (Which tree), KindsAtIndices indices tree ~ branch)

data CaseDiversifyN (indices :: [Nat]) (n :: Nat) (branch' :: [Type]) r = CaseDiversifyN

instance ReiterateN (CaseDiversifyN indices) n branch' where
    reiterateN CaseDiversifyN = CaseDiversifyN

instance MemberAt (KindAtIndex n indices) x tree =>
         Case (CaseDiversifyN indices n) (x ': branch') (Which tree) where
    case' CaseDiversifyN v = pickN (Proxy @(KindAtIndex n indices)) v

------------------------------------------------------------------

-- | Convert a Which into possibly another Which with a totally different typelist.
-- Returns either the 'Right' reinterpretation, or the 'Left' over Which type.
-- NB. forall used to specify @branch@ first, so TypeApplications can be used to specify @branch@.
reinterpret :: forall branch tree. Reinterpret branch tree => Which tree -> Either (Which (Complement tree branch)) (Which branch)
reinterpret = which (CaseReinterpret @branch @tree @tree)

type Reinterpret branch tree = (Reduce Which (Switch (CaseReinterpret branch tree)) tree (Either (Which (Complement tree branch)) (Which branch))
         , IsDistinct branch
         , IsDistinct tree)

data CaseReinterpret (branch :: [Type]) (tree :: [Type]) (tree' :: [Type]) r = CaseReinterpret

instance Reiterate (CaseReinterpret branch tree) tree' where
    reiterate CaseReinterpret = CaseReinterpret

instance ( MaybeUniqueMember x branch
         , comp ~ Complement tree branch
         , MaybeUniqueMember x comp
         ) =>
         Case (CaseReinterpret branch tree) (x ': tree') (Either (Which comp) (Which branch)) where
    case' CaseReinterpret a =
        case fromInteger (natVal @(PositionOf x branch) Proxy) of
            0 -> let j = fromInteger (natVal @(PositionOf x (Complement tree branch)) Proxy)
                 -- safe use of partial! j will never be zero due to check above
                 in Left $ Which (j - 1) (unsafeCoerce a)
            i -> Right $ Which (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | A limited form of 'reinterpret' where the @branch@ must be a subset of @tree@ instead of any arbitrary Which.
-- Also it returns a Maybe instead of Either.
-- This is so that the @indices@ can be the same as in 'narrowN'.
-- Specify a typelist mapping of @branch@ into @tree@
-- where indices[branch_idx] = tree_idx
reinterpretN' :: forall (indices :: [Nat]) branch tree proxy. (ReinterpretN indices branch tree) => proxy indices -> Which tree -> Maybe (Which branch)
reinterpretN' _ = whichN (CaseReinterpretN @indices @0 @tree)

type ReinterpretN (indices :: [Nat]) (branch :: [Type]) (tree :: [Type]) = (Reduce Which (SwitchN (CaseReinterpretN indices) 0) tree (Maybe (Which (KindsAtIndices indices tree))), KindsAtIndices indices tree ~ branch)

data CaseReinterpretN (indices :: [Nat]) (n :: Nat) (tree' :: [Type]) r = CaseReinterpretN

instance ReiterateN (CaseReinterpretN indices) n tree' where
    reiterateN CaseReinterpretN = CaseReinterpretN

instance MaybeMemberAt (PositionOf n indices) x branch => Case (CaseReinterpretN indices n) (x ': tree) (Maybe (Which branch)) where
    case' CaseReinterpretN a =
        case fromInteger (natVal @(PositionOf n indices) Proxy) of
            0 -> Nothing
            i -> Just $ Which (i - 1) (unsafeCoerce a)

-- ------------------------------------------------------------------

-- | Injection.
-- A Which can be 'diversify'ed to contain more types or 'reinterpret'ed into possibly another Which type.
-- Use TypeApplication to specify the containing @reinterpreted@ type of the prism.
--
-- @inject \@[Int, Bool]@
--
-- Use @_ to specify the @reinterpreted@ typelist instead.
--
-- @inject \@_ \@'[Int, String]@
inject
    :: forall branch tree.
       ( Diversify tree branch
       , Reinterpret branch tree
       )
    => Prism' (Which tree) (Which branch)
inject = prism' diversify (hush . reinterpret)
{-# INLINE inject #-}

injectN
    :: forall indices branch tree proxy.
       ( DiversifyN indices tree branch
       , ReinterpretN indices branch tree
       )
    => proxy indices -> Prism' (Which tree) (Which branch)
injectN p = prism' (diversifyN p) (reinterpretN' p)
{-# INLINE injectN #-}

------------------------------------------------------------------

-- | 'Switch' is an instance of 'Reduce' for which __'reiterate'__s through the possibilities in a 'Which',
-- delegating work to 'Case', ensuring termination when Which only contains one type.
newtype Switch c (xs :: [Type]) r = Switch (c xs r)

-- | 'trial'' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterate'__
-- trying the next type in the type list.
-- This code will be efficiently compiled into a single case statement in GHC 8.2.1
-- See http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html
instance (Case c (x ': x' ': xs) r, Reduce Which (Switch c) (x' ': xs) r, Reiterate c (x : x' : xs)) =>
         Reduce Which (Switch c) (x ': x' ': xs) r where
    reduce (Switch c) v =
        case trial' v of
            Right a -> case' c a
            Left v' -> reduce (Switch (reiterate c)) v'
    {-# INLINE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
-- You can't reduce 'impossible'
instance (Case c '[x] r) => Reduce Which (Switch c) '[x] r where
    reduce (Switch c) v = case obvious v of
            a -> case' c a

-- | Catamorphism for 'Which'. This is equivalent to @flip switch@.
which :: Reduce Which (Switch case') xs r => case' xs r -> Which xs -> r
which = reduce . Switch

-- | A switch/case statement for 'Which'. This is equivalent to @flip which@
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Which' of functions to a variant of values.
-- Or 'Data.Diverse.CaseTypeable.CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
-- Or you may use your own custom instance of 'Case'.
switch :: Reduce Which (Switch case') xs r => Which xs -> case' xs r -> r
switch = flip which

------------------------------------------------------------------

-- | 'Switch' is an instance of 'Reduce' for which __'reiterateN'__s through the possibilities in a 'Which',
-- delegating work to 'CaseN', ensuring termination when 'Which' only contains one type.
newtype SwitchN c (n :: Nat) (xs :: [Type]) r = SwitchN (c n xs r)

-- | 'trial'' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterateN'__
-- trying the next type in the type list.
-- This code will be efficiently compiled into a single case statement in GHC 8.2.1
-- See http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html
instance (Case (c n) (x ': x' ': xs) r, Reduce Which (SwitchN c (n + 1)) (x' ': xs) r, ReiterateN c n (x : x' : xs)) =>
         Reduce Which (SwitchN c n) (x ': x' ': xs) r where
    reduce (SwitchN c) v =
        case trial' v of
            Right a -> case' c a
            Left v' -> reduce (SwitchN (reiterateN c)) v'
    {-# INLINE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
-- You can't reduce 'impossible'
instance (Case (c n) '[x] r) => Reduce Which (SwitchN c n) '[x] r where
    reduce (SwitchN c) v = case obvious v of
            a -> case' c a

-- | Catamorphism for 'Which'. This is equivalent to @flip switch@.

whichN :: Reduce Which (SwitchN case' n) xs r => case' n xs r -> Which xs -> r
whichN = reduce . SwitchN

-- | A switch/case statement for Which.
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Which' of functions to a variant of values.
-- Or 'Data.Diverse.CaseTypeable.CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
-- Or you may use your own custom instance of 'Case'.
switchN :: Reduce Which (SwitchN case' n) xs r => Which xs -> case' n xs r -> r
switchN = flip whichN

-----------------------------------------------------------------

instance (Reduce Which (Switch CaseEqWhich) (x ': xs) Bool) => Eq (Which (x ': xs)) where
    l@(Which i _) == (Which j u) =
        if i /= j
            then False
            else switch l (CaseEqWhich u)

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

instance (Reduce Which (Switch CaseEqWhich) (x ': xs) Bool, Reduce Which (Switch CaseOrdWhich) (x ': xs) Ordering) => Ord (Which (x ': xs)) where
    compare l@(Which i _) (Which j u) =
        if i /= j
            then compare i j
            else switch l (CaseOrdWhich u)

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

instance (Reduce Which (Switch CaseShowWhich) (x ': xs) ShowS) => Show (Which (x ': xs)) where
    showsPrec d v = showParen (d > app_prec) ((showString "pick ") . (which CaseShowWhich v))
      where app_prec = 10

instance Show (Which '[]) where
    showsPrec d _ = showParen (d > app_prec) (showString "impossible")
      where app_prec = 10

data CaseShowWhich (xs :: [Type]) r = CaseShowWhich

instance Reiterate CaseShowWhich (x ': xs) where
    reiterate CaseShowWhich = CaseShowWhich

instance Show x => Case CaseShowWhich (x ': xs) ShowS where
    case' _ = showsPrec (app_prec + 1)
      where app_prec = 10

------------------------------------------------------------------

newtype EmitReadWhich (xs :: [Type]) r = EmitReadWhich Int

instance Reiterate EmitReadWhich (x ': xs) where
    reiterate (EmitReadWhich i) = EmitReadWhich (i + 1)

instance Read x => Emit EmitReadWhich (x ': xs) (ReadPrec (Int, WrappedAny)) where
    emit (EmitReadWhich i) = (\a -> (i, WrappedAny (unsafeCoerce a))) <$> readPrec @x

readWhich
    :: forall xs.
       AFoldable (Collector EmitReadWhich xs) (ReadPrec (Int, WrappedAny))
    => Proxy (xs :: [Type]) -> ReadPrec (Int, WrappedAny)
readWhich _ = afoldr (<|>) empty (Collector (EmitReadWhich @xs 0))

-- | This 'Read' instance tries to read using the each type in the typelist, using the first successful type read.
instance ( IsDistinct (x ': xs)
         , AFoldable (Collector EmitReadWhich (x ': xs)) (ReadPrec (Int, WrappedAny))
         ) =>
         Read (Which (x ': xs)) where
    readPrec =
        parens $
        prec 10 $ do
            lift $ L.expect (Ident "pick")
            (n, WrappedAny v) <- step (readWhich @(x ': xs) Proxy)
            pure (Which n v)

instance Read (Which '[]) where
    readPrec =
        parens $
        prec 10 $ do
            lift $ L.expect (Ident "impossible")
            pure impossible

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any
