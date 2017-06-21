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

module Data.Diverse.Many.Internal (
      -- * 'Many' type
      Many(..) -- exporting constructor unsafely!

      -- * Single type
      -- ** Construction
    , pick
    , pick'
    , pickN
      -- ** Destruction
    , notMany
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
    , forMany
    , switch
    , SwitchN(..)
    , forManyN
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

-- | A Many is an anonymous sum type (also known as a polymorphic variant, or co-record)
-- that has only distincs types in the list of possible types.
-- That is, there are no duplicates types in the possibilities of this type.
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to try values in the Many.
-- This is essentially a typed version of 'Data.Dynamic'
--
-- Mnemonic: It doesn't contain one of 'Any' type, it contains one of of 'Many' types.
--
-- Encoding: The variant contains a value whose type is at the given position in the type list.
-- This is similar to the encoding as Haskus.Util.Many and HList (which used Int instead of Word)
-- but with a different api.
-- See https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Int Any

-- Just like Haskus and HList versions, inferred type is phamtom which is wrong
type role Many representational

-- Not using GADTs with the Distinct constraint as it gets in the way when I know something is Distinct,
-- but I don't know how to prove it to GHC. Eg a subset of something Distinct is also Distinct...
-- data Many (xs :: [Type]) where
--     Many :: (Distinct xs) => {-# UNPACK #-} !Int -> Any -> Many xs

-- NB. nominal is required for GADTs with constraints
-- type role Many nominal

----------------------------------------------

-- | Lift a value into a Many of possibly other types.
-- NB. forall used to specify @xs@ first, so TypeApplications can be used to specify @xs@.
pick :: forall xs x. (Distinct xs, KnownNat (IndexOf x xs)) => x -> Many xs
pick = unsafeToMany

-- | A variation of 'pick' into a Many of a single type
pick' :: x -> Many '[x]
pick' = pick

-- | Lift a value into a Many of possibly other (possibley indistinct) types, where the value is the n-th type.
--
-- @pickN \@0 @'[Int, Bool, Char] Proxy 5@
pickN :: forall n xs x proxy. (KnownNat n, x ~ KindAtIndex n xs) => proxy n -> x -> Many xs
pickN _ = Many (fromInteger (natVal @n Proxy)) . unsafeCoerce

-- | Internal function to create a many without bothering with the 'Distinct' constraint
-- This is useful when we know something is Distinct, but I don't know how (or can't be bothered)
-- to prove it to GHC.
-- Eg. a subset of something Distinct is also Distinct.
unsafeToMany :: forall x xs. (KnownNat (IndexOf x xs)) => x -> Many xs
unsafeToMany = Many (fromInteger (natVal @(IndexOf x xs) Proxy)) . unsafeCoerce

-- | Retrieving the value out of a 'Many' of one type is always successful.
-- Mnemonic: A 'Many' with one type is 'notMany' at all.
notMany :: Many '[a] -> a
notMany (Many _ v) = unsafeCoerce v

-- | 'trial' a value out of a Many, and get Either the Right value or the Left-over possibilities.
trial
    :: forall x xs.
       (Distinct xs, KnownNat (IndexOf x xs))
    => Many xs -> Either (Many (Without x xs)) x
trial (Many n v) = let i = fromInteger (natVal @(IndexOf x xs) Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Many (n - 1) v)
                          else Left (Many n v)

-- | A version of 'trial' which trys the first type in the type list.
trial' :: Many (x ': xs) -> Either (Many xs) x
trial' (Many n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Many (n - 1) v)


-- | 'trialN' a value out of a Many, and get Either the Right value or the Left-over possibilities.
trialN
    :: forall n xs x proxy.
       (KnownNat n, x ~ KindAtIndex n xs)
    => proxy n -> Many xs -> Either (Many (Without x xs)) x
trialN _ (Many n v) = let i = fromInteger (natVal @n Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Many (n - 1) v)
                          else Left (Many n v)

-- | Utility to convert Either to Maybe
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-----------------------------------------------------------------

-- | A Many has a prism to an the inner type.
-- That is, a value can be 'pick'ed into a Many or mabye 'trial'ed out of a Many.
-- Use TypeApplication to specify the inner type of the of the prism.
-- Example: @facet \@Int@
facet :: forall x xs. (Distinct xs, KnownNat (IndexOf x xs)) => Prism' (Many xs) x
facet = prism' pick (hush . trial)
{-# INLINE facet #-}

facetN :: forall n xs x proxy. (KnownNat n, x ~ KindAtIndex n xs) => proxy n -> Prism' (Many xs) x
facetN p = prism' (pickN p) (hush . trialN p)
{-# INLINE facetN #-}

------------------------------------------------------------------

-- | Convert a Many to another Many that may include other possibilities.
-- That is, @branch@ is equal or is a subset of @tree@.
-- This can be used to rearrange the order of the types in the Many.
-- NB. @tree@ is ordered first, so TypeApplications can be used to specify it.
diversify :: forall tree branch. Diversify tree branch => Many branch -> Many tree
diversify = forMany (CaseDiversify @tree @branch)

-- | A friendlier constraint synonym for 'diversify'. All 'Many' fufill this constraint.
type Diversify (tree :: [Type]) (branch :: [Type]) = (Reduce Many (Switch (CaseDiversify tree)) branch (Many tree), Distinct tree)

data CaseDiversify (tree :: [Type]) (branch' :: [Type]) r = CaseDiversify

instance Reiterate (CaseDiversify tree) branch' where
    reiterate CaseDiversify = CaseDiversify

instance (KnownNat (IndexOf x tree)) =>
         Case (CaseDiversify tree) (x ': branch') (Many tree) where
    case' CaseDiversify = unsafeToMany

------------------------------------------------------------------

-- where indices[branch_idx] = tree_idx

diversifyN :: forall indices tree branch proxy. (DiversifyN indices tree branch) => proxy indices -> Many branch -> Many tree
diversifyN _ = forManyN (CaseDiversifyN @indices @0 @branch)

type DiversifyN (indices :: [Nat]) (tree :: [Type]) (branch :: [Type]) = (Reduce Many (SwitchN (CaseDiversifyN indices) 0) (KindsAtIndices indices tree) (Many tree), KindsAtIndices indices tree ~ branch)

data CaseDiversifyN (indices :: [Nat]) (n :: Nat) (branch' :: [Type]) r = CaseDiversifyN

instance ReiterateN (CaseDiversifyN indices) n branch' where
    reiterateN CaseDiversifyN = CaseDiversifyN

instance ( KnownNat (KindAtIndex n indices)
         , x ~ KindAtIndex (KindAtIndex n indices) tree
         ) =>
         Case (CaseDiversifyN indices n) (x ': branch') (Many tree) where
    case' CaseDiversifyN v = pickN (Proxy @(KindAtIndex n indices)) v

------------------------------------------------------------------

-- | Convert a Many into possibly another Many with a totally different typelist.
-- Returns either the 'Right' reinterpretation, or the 'Left' over Many type.
-- NB. forall used to specify @branch@ first, so TypeApplications can be used to specify @branch@.
reinterpret :: forall branch tree. Reinterpret branch tree => Many tree -> Either (Many (Complement tree branch)) (Many branch)
reinterpret = forMany (CaseReinterpret @branch @tree @tree)

type Reinterpret branch tree = (Reduce Many (Switch (CaseReinterpret branch tree)) tree (Either (Many (Complement tree branch)) (Many branch))
         , Distinct branch
         , Distinct tree)

data CaseReinterpret (branch :: [Type]) (tree :: [Type]) (tree' :: [Type]) r = CaseReinterpret

instance Reiterate (CaseReinterpret branch tree) tree' where
    reiterate CaseReinterpret = CaseReinterpret

instance ( KnownNat (PositionOf x branch)
         , comp ~ Complement tree branch
         , KnownNat (PositionOf x comp)
         ) =>
         Case (CaseReinterpret branch tree) (x ': tree') (Either (Many comp) (Many branch)) where
    case' CaseReinterpret a =
        case fromInteger (natVal @(PositionOf x branch) Proxy) of
            0 -> let j = fromInteger (natVal @(PositionOf x (Complement tree branch)) Proxy)
                 in Left $ Many (j - 1) (unsafeCoerce a) -- j will never be zero
            i -> Right $ Many (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | A limited form of 'reinterpret' where the @branch@ must be a subset of @tree@ instead of any arbitrary Many.
-- Also it returns a Maybe instead of Either.
-- Specify a typelist mapping of @branch@ into @tree@
-- where indices[branch_idx] = tree_idx
reinterpretN' :: forall (indices :: [Nat]) branch tree proxy. (ReinterpretN indices branch tree) => proxy indices -> Many tree -> Maybe (Many branch)
reinterpretN' _ = forManyN (CaseReinterpretN @indices @0 @tree)

-- | A friendllier constraint synonym for 'reinterpret'. All 'Many' fufill this constraint.
type ReinterpretN (indices :: [Nat]) (branch :: [Type]) (tree :: [Type]) = (Reduce Many (SwitchN (CaseReinterpretN indices) 0) tree (Maybe (Many (KindsAtIndices indices tree))), KindsAtIndices indices tree ~ branch)

data CaseReinterpretN (indices :: [Nat]) (n :: Nat) (tree' :: [Type]) r = CaseReinterpretN

instance ReiterateN (CaseReinterpretN indices) n tree' where
    reiterateN CaseReinterpretN = CaseReinterpretN

instance ( KnownNat (PositionOf n indices)
         , x ~ KindAtIndex n branch) => Case (CaseReinterpretN indices n) (x ': tree) (Maybe (Many branch)) where
    case' CaseReinterpretN a =
        case fromInteger (natVal @(PositionOf n indices) Proxy) of
            0 -> Nothing
            i -> Just $ Many (i - 1) (unsafeCoerce a)

-- ------------------------------------------------------------------

-- | Injection.
-- A Many can be 'diversify'ed to contain more types or 'reinterpret'ed into possibly another Many type.
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
    => Prism' (Many tree) (Many branch)
inject = prism' diversify (hush . reinterpret)
{-# INLINE inject #-}

injectN
    :: forall indices branch tree proxy.
       ( DiversifyN indices tree branch
       , ReinterpretN indices branch tree
       )
    => proxy indices -> Prism' (Many tree) (Many branch)
injectN p = prism' (diversifyN p) (reinterpretN' p)
{-# INLINE injectN #-}

------------------------------------------------------------------

-- | 'Switch' is an instance of 'Reduce' for which visits through the possibilities in Many,
-- delegating work to 'Case', ensuring termination when Many only contains one type.
newtype Switch c (xs :: [Type]) r = Switch (c xs r)

-- | 'trial' each type in a Many, and either case' the handling of the value discovered, or loop
-- trying the next type in the type list.
-- This code will be efficiently compiled into a single case statement in GHC 8.2.1
-- See http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html
instance (Case c (x ': x' ': xs) r, Reduce Many (Switch c) (x' ': xs) r, Reiterate c (x : x' : xs)) =>
         Reduce Many (Switch c) (x ': x' ': xs) r where
    reduce (Switch c) v =
        case trial' v of
            Right a -> case' c a
            Left v' -> reduce (Switch (reiterate c)) v'
    {-# INLINE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
instance (Case c '[x] r) => Reduce Many (Switch c) '[x] r where
    reduce (Switch c) v = case notMany v of
            a -> case' c a

-- | Catamorphism for 'Many'. This is equivalent to @flip switch@.
forMany :: Reduce Many (Switch case') xs r => case' xs r -> Many xs -> r
forMany = reduce . Switch

-- | A switch/case statement for Many.
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Nary' of functions to a variant of values.
-- Or 'Data.Diverse.CaseTypeable.CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
-- Or you may use your own custom instance of 'Case'.
switch :: Reduce Many (Switch case') xs r => Many xs -> case' xs r -> r
switch = flip forMany

------------------------------------------------------------------

newtype SwitchN c (n :: Nat) (xs :: [Type]) r = SwitchN (c n xs r)

instance (Case (c n) (x ': x' ': xs) r, Reduce Many (SwitchN c (n + 1)) (x' ': xs) r, ReiterateN c n (x : x' : xs)) =>
         Reduce Many (SwitchN c n) (x ': x' ': xs) r where
    reduce (SwitchN c) v =
        case trial' v of
            Right a -> case' c a
            Left v' -> reduce (SwitchN (reiterateN c)) v'
    {-# INLINE reduce #-}

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
instance (Case (c n) '[x] r) => Reduce Many (SwitchN c n) '[x] r where
    reduce (SwitchN c) v = case notMany v of
            a -> case' c a

-- | Catamorphism for 'Many'. This is equivalent to @flip switch@.
forManyN :: Reduce Many (SwitchN case' n) xs r => case' n xs r -> Many xs -> r
forManyN = reduce . SwitchN

-- | A switch/case statement for Many.
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Nary' of functions to a variant of values.
-- Or 'Data.Diverse.CaseTypeable.CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
-- Or you may use your own custom instance of 'Case'.
switchN :: Reduce Many (SwitchN case' n) xs r => Many xs -> case' n xs r -> r
switchN = flip forManyN

-----------------------------------------------------------------

instance (Reduce Many (Switch CaseEqMany) xs Bool) => Eq (Many xs) where
    l@(Many i _) == (Many j u) =
        if i /= j
            then False
            else switch l (CaseEqMany u)

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseEqMany (xs :: [Type]) r = CaseEqMany Any

instance Reiterate CaseEqMany (x ': xs) where
    reiterate (CaseEqMany r) = CaseEqMany r

instance (Eq x) => Case CaseEqMany (x ': xs) Bool where
    case' (CaseEqMany r) l = l == unsafeCoerce r

-----------------------------------------------------------------

instance (Reduce Many (Switch CaseEqMany) xs Bool, Reduce Many (Switch CaseOrdMany) xs Ordering) => Ord (Many xs) where
    compare l@(Many i _) (Many j u) =
        if i /= j
            then compare i j
            else switch l (CaseOrdMany u)

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseOrdMany (xs :: [Type]) r = CaseOrdMany Any

instance Reiterate CaseOrdMany (x ': xs) where
    reiterate (CaseOrdMany r) = CaseOrdMany r

instance (Ord x) => Case CaseOrdMany (x ': xs) Ordering where
    case' (CaseOrdMany r) l = compare l (unsafeCoerce r)

------------------------------------------------------------------

instance (Reduce Many (Switch CaseShowMany) xs ShowS) => Show (Many xs) where
    showsPrec d v = showParen (d > app_prec) ((showString "Many ") . (forMany CaseShowMany v))
      where app_prec = 10

data CaseShowMany (xs :: [Type]) r = CaseShowMany

instance Reiterate CaseShowMany (x ': xs) where
    reiterate CaseShowMany = CaseShowMany

instance Show x => Case CaseShowMany (x ': xs) ShowS where
    case' _ = showsPrec (app_prec + 1)
      where app_prec = 10

------------------------------------------------------------------

newtype EmitReadMany (xs :: [Type]) r = EmitReadMany Int

instance Reiterate EmitReadMany (x ': xs) where
    reiterate (EmitReadMany i) = EmitReadMany (i + 1)

instance Read x => Emit EmitReadMany (x ': xs) (ReadPrec (Int, WrappedAny)) where
    emit (EmitReadMany i) = (\a -> (i, WrappedAny (unsafeCoerce a))) <$> readPrec @x

readMany
    :: forall xs.
       AFoldable (Collector EmitReadMany xs) (ReadPrec (Int, WrappedAny))
    => Proxy (xs :: [Type]) -> ReadPrec (Int, WrappedAny)
readMany _ = afoldr (<|>) empty (Collector (EmitReadMany @xs 0))

-- | This 'Read' instance tries to read using the each type in the typelist, using the first successful type read.
instance ( Distinct xs
         , AFoldable (Collector EmitReadMany xs) (ReadPrec (Int, WrappedAny))
         ) =>
         Read (Many xs) where
    readPrec =
        parens $
        prec 10 $ do
            lift $ L.expect (Ident "Many")
            (n, WrappedAny v) <- step (readMany @xs Proxy)
            pure (Many n v)

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any
