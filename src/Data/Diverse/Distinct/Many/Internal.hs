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

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Diverse.Distinct.Many.Internal where

import Control.Applicative
import Control.Lens
import Data.Diverse.Class.AFoldable
import Data.Diverse.Class.Case
import Data.Diverse.Class.Emit
import Data.Diverse.Class.Reiterate
import Data.Diverse.Data.Assemble
import Data.Diverse.Data.WrappedAny
import Data.Diverse.Distinct.Catalog
import Data.Diverse.Type
import Data.Kind
import Data.Proxy
import Data.Typeable
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
-- Mnemonic: It doesn't contain one of Any type, it contains one of of Many types.
--
-- Encoding: The variant contains a value whose type is at the given position in the type list.
-- This is similar to the encoding as Haskus.Util.Many and HList (which used Int instead of Word)
-- but with a different api.
-- See https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Word Any

-- | Just like Haskus and HList versions, inferred type is phamtom which is wrong
type role Many representational

-- Not using GADTs with the Distinct constraint as it gets in the way when I know something is Distinct,
-- but I don't know how to prove it to GHC. Eg a subset of something Distinct is also Distinct...
-- data Many (xs :: [Type]) where
--     Many :: (Distinct xs) => {-# UNPACK #-} !Word -> Any -> Many xs

-- NB. nominal is required for GADTs with constraints
-- type role Many nominal

----------------------------------------------

-- | Lift a value into a Many of possibly other types.
-- NB. forall used to specify xs first, so TypeApplications can be used to specify xs.
pick :: forall xs x. (Distinct xs, Member x xs) => x -> Many xs
pick = Many (fromIntegral (natVal @(IndexOf x xs) Proxy)) . unsafeCoerce

-- | A variation of 'pick' into a Many of a single type
pick' :: x -> Many '[x]
pick' = pick

-- | Internal function to create a many without bothering with the 'Distinct' constraint
-- This is useful when we know something is Distinct, but I don't know how (or can't be bothered)
-- to prove it to GHC.
-- Eg. a subset of something Distinct is also Distinct.
-- unsafeToMany :: forall x xs. (Member x xs) => x -> Many xs
-- unsafeToMany = Many (fromIntegral (natVal @(IndexOf x xs) Proxy)) . unsafeCoerce

-- | Retrieving the value out of a 'Many' of one type is always successful.
-- Mnemonic: A 'Many' with one type is 'notMany' at all.
notMany :: Many '[a] -> a
notMany (Many _ v) = unsafeCoerce v

-- | For a specified or inferred type, deconstruct a Many into a Maybe value of that type.
trial :: forall x xs. (Member x xs) => Many xs -> Maybe x
trial (Many n v) = if n == fromIntegral (natVal @(IndexOf x xs) Proxy)
            then Just (unsafeCoerce v)
            else Nothing

-- | A version of 'trial'' which trys the first type in the type list.
trial' :: Many (x ': xs) -> Maybe x
trial' (Many n v) = if n == 0
            then Just (unsafeCoerce v)
            else Nothing

-- | 'trial' a value out of a Many, and get Either the Right value or the Left-over possibilities.
trialEither
    :: forall x xs.
       (Member x xs)
    => Many xs -> Either (Many (Without x xs)) x
trialEither (Many n v) = let i = fromIntegral (natVal @(IndexOf x xs) Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Many (n - 1) v)
                          else Left (Many n v)

-- | A version of 'trialEither' which trys the first type in the type list.
trialEither' :: Many (x ': xs) -> Either (Many xs) x
trialEither' (Many n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Many (n - 1) v)

------------------------------------------------------------------

-- | A Many has a prism to an the inner type.
-- That is, a value can be 'pick'ed into a Many or mabye 'trial'ed out of a Many.
-- Use TypeApplication to specify the inner type of the of the prism.
-- Example: @facet \@Int@
facet :: forall x xs. (Distinct xs, Member x xs) => Prism' (Many xs) x
facet = prism' pick trial
{-# INLINE facet #-}

------------------------------------------------------------------

-- | Convert a Many to another Many that may include other possibilities.
-- That is, xs is equal or is a subset of ys.
-- Can be used to rearrange the order of the types in the Many.
-- NB. forall used to specify ys first, so TypeApplications can be used to specify ys.
-- The Switch constraint is fulfilled with
-- (Distinct ys, forall x (in xs). Member x xs)
diversify :: forall ys xs. Switch (CaseDiversify ys) xs (Many ys) => Many xs -> Many ys
diversify = foldMany (CaseDiversify @ys)

data CaseDiversify (ys :: [Type]) (xs :: [Type]) r = CaseDiversify

instance Reiterate (CaseDiversify ys) xs where
    reiterate CaseDiversify = CaseDiversify

instance (Member (Head xs) ys, Distinct ys) => Case (CaseDiversify ys) xs (Many ys) where
    then' CaseDiversify = pick

------------------------------------------------------------------

-- | Convert a Many into possibly another Many with a totally different typelist.
-- NB. forall used to specify ys first, so TypeApplications can be used to specify ys.
-- The Switch constraint is fulfilled with
-- (Distinct ys, forall x (in xs). (MaybeMember x ys)
reinterpret :: forall ys xs. Switch (CaseReinterpret ys) xs (Maybe (Many ys)) => Many xs -> Maybe (Many ys)
reinterpret = foldMany (CaseReinterpret @ys)

data CaseReinterpret (ys :: [Type]) (xs :: [Type]) r = CaseReinterpret

instance Reiterate (CaseReinterpret ys) xs where
    reiterate CaseReinterpret = CaseReinterpret

instance (MaybeMember (Head xs) ys, Distinct ys) => Case (CaseReinterpret ys) xs (Maybe (Many ys)) where
    then' CaseReinterpret a = case fromIntegral (natVal @(PositionOf (Head xs) ys) Proxy) of
                                     0 -> Nothing
                                     i -> Just $ Many (i - 1) (unsafeCoerce a)


-- | Like reinterpret, but return the Complement (the parts of xs not in ys) in the case of failure.
-- The Switch constraints is fulfilled with
-- (Distinct ys, forall x (in xs). (MaybeMember x ys, Member x (Complement xs ys)))
reinterpretEither
    :: forall ys xs.
       ( Switch (CaseDiversify (Complement xs ys)) xs (Many (Complement xs ys))
       , Switch (CaseReinterpret ys) xs (Maybe (Many ys))
       )
    => Many xs -> Either (Many (Complement xs ys)) (Many ys)
reinterpretEither v = case reinterpret v of
    Nothing -> Left (diversify v)
    Just v' -> Right v'

------------------------------------------------------------------

-- | Injection.
-- A Many can be 'diversify'ed to contain more types or 'reinterpret'ed into possibly another Many type.
-- This typeclass looks like 'Facet' but is used for different purposes.
-- Use TypeApplication to specify the containing 'diversified' type of the prism.
-- Example: @inject \@[Int, Bool]@
inject
    :: forall tree branch.
       ( (Switch (CaseDiversify tree) branch (Many tree))
       , (Switch (CaseReinterpret branch) tree (Maybe (Many branch)))
       )
    => Prism' (Many tree) (Many branch)
inject = prism' diversify reinterpret
{-# INLINE inject #-}

-- | A variation of inject with the type parameters reorderd,
-- so that TypeApplications can be used to specify the contained 'reinterpreted' type of the prism
injected
    :: forall branch tree.
       ( (Switch (CaseDiversify tree) branch (Many tree))
       , (Switch (CaseReinterpret branch) tree (Maybe (Many branch)))
       )
    => Prism' (Many tree) (Many branch)
injected = inject
{-# INLINE injected #-}

------------------------------------------------------------------

-- | A switch/case statement for Many.
-- Use 'Case' instances like 'Cases' to apply a 'Catalog' of functions to a variant of values.
-- Or 'TypeableCase' to apply a polymorphic function that work on all 'Typeables'.
-- Or you may use your own custom instance of 'Case'.
class Switch handler (xs :: [Type]) r where
    switch :: Many xs -> handler xs r -> r

-- | This instance of 'Switch' for which visits through the possibilities in Many,
-- delegating work to 'Case', ensuring termination when Many only contains one type.
-- 'trial' each type in a Many, and either then' the handling of the value discovered, or loop
-- trying the next type in the type list.
-- This code will be efficiently compiled into a single case statement in GHC 8.2.1
-- See http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html

newtype Switcher c (xs :: [Type]) r = Switcher (c xs r)

instance (Case c (x ': x' ': xs) r, Switch (Switcher c) (x' ': xs) r, Reiterate c (x : x' : xs)) =>
         Switch (Switcher c) (x ': x' ': xs) r where
    switch v (Switcher c) =
        case trialEither' v of
            Right a -> then' c a
            Left v' -> switch v' (Switcher (reiterate c))
    {-# INLINE switch #-}

-- | Terminating case of the loop, ensuring that a instance of @Switch '[]@
-- with an empty typelist is not required.
instance (Case c '[x] r) => Switch (Switcher c) '[x] r where
    switch v (Switcher c) = case notMany v of
            a -> then' c a

-- | Catamorphism for many. This is @flip switch@. See also 'Switch'
foldMany :: Switch handler xs r => handler xs r -> Many xs -> r
foldMany = flip switch

-------------------------------------------

-- | Contains a 'Catalog' of handlers/continuations for all thypes in the 'xs' typelist.
newtype Cases (fs :: [Type]) (xs :: [Type]) r = Cases (Catalog fs)

instance Reiterate (Cases fs) xs where
    reiterate (Cases s) = Cases s

-- | An instance of 'Case' that can be 'Switch'ed where it contains a 'Catalog' of handlers/continuations
-- for all thypes in the 'xs' typelist.
instance (Item (Head xs -> r) (Catalog fs)) => Case (Cases fs) xs r where
    then' (Cases s) = s ^. item

-- | Create Cases for handling 'switch' from a tuple.
-- This function imposes additional constraints than using 'Cases' constructor directly:
-- * SameLength constraints to prevent human confusion with unusable cases.
-- * OutcomeOf fs ~ r constraints to ensure that the Catalog only continutations that return r.
-- Example: @switch a $ cases (f, g, h)@
cases :: (SameLength fs xs, OutcomeOf fs ~ r, Cataloged fs, fs ~ TypesOf (TupleOf fs)) => TupleOf fs -> Switcher (Cases fs) xs r
cases = Switcher . Cases . catalog

-------------------------------------------

-- | This handler stores a polymorphic function for all Typeables.
newtype CaseTypeable (xs :: [Type]) r = CaseTypeable (forall x. Typeable x => x -> r)

instance Reiterate CaseTypeable xs where
    reiterate (CaseTypeable f) = CaseTypeable f

instance Typeable (Head xs) => Case CaseTypeable xs r where
    then' (CaseTypeable f) = f

-----------------------------------------------------------------

instance (Switch CaseEqMany xs Bool) => Eq (Many xs) where
    l@(Many i _) == (Many j u) =
        if i /= j
            then False
            else switch l (CaseEqMany u)

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseEqMany (xs :: [Type]) r = CaseEqMany Any

instance Reiterate CaseEqMany xs where
    reiterate (CaseEqMany r) = CaseEqMany r

instance (Eq (Head xs)) => Case CaseEqMany xs Bool where
    then' (CaseEqMany r) l = l == unsafeCoerce r

-----------------------------------------------------------------

instance (Switch CaseEqMany xs Bool, Switch CaseOrdMany xs Ordering) => Ord (Many xs) where
    compare l@(Many i _) (Many j u) =
        if i /= j
            then compare i j
            else switch l (CaseOrdMany u)

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseOrdMany (xs :: [Type]) r = CaseOrdMany Any

instance Reiterate CaseOrdMany xs where
    reiterate (CaseOrdMany r) = CaseOrdMany r

instance (Ord (Head xs)) => Case CaseOrdMany xs Ordering where
    then' (CaseOrdMany r) l = compare l (unsafeCoerce r)

------------------------------------------------------------------

instance (Switch CaseShowMany xs ShowS) => Show (Many xs) where
    showsPrec d v = showParen (d >= 11) ((showString "Many ") . (foldMany (CaseShowMany 11) v))

newtype CaseShowMany (xs :: [Type]) r = CaseShowMany Int

instance Reiterate CaseShowMany xs where
    reiterate (CaseShowMany d) = CaseShowMany d

instance Show (Head xs) => Case CaseShowMany xs ShowS where
    then' (CaseShowMany d) = showsPrec d

------------------------------------------------------------------

newtype EmitReadMany (xs :: [Type]) r = EmitReadMany Word

instance Reiterate EmitReadMany (x ': xs) where
    reiterate (EmitReadMany i) = EmitReadMany (i + 1)

instance Read x => Emit EmitReadMany (x ': xs) (ReadPrec (Word, WrappedAny)) where
    emit (EmitReadMany i) = (\a -> (i, WrappedAny (unsafeCoerce a))) <$> readPrec @x

readMany
    :: forall xs.
       AFoldable (Assemble EmitReadMany xs) (ReadPrec (Word, WrappedAny))
    => Proxy (xs :: [Type]) -> ReadPrec (Word, WrappedAny)
readMany _ = afoldr (<|>) empty (Assemble (EmitReadMany @xs 0))

-- | This 'Read' instance tries to read using the each type in the typelist, using the first successful type read.
instance ( Distinct xs
         , AFoldable (Assemble EmitReadMany xs) (ReadPrec (Word, WrappedAny))
         ) =>
         Read (Many xs) where
    readPrec =
        parens $
        prec 10 $ do
            lift $ L.expect (Ident "Many")
            (n, WrappedAny v) <- step (readMany @xs Proxy)
            pure (Many n v)
