{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.ManySpec (main, spec) where

import Data.Diverse
import Data.Int
import Data.Tagged
import Data.Typeable
import Test.Hspec


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec dicovery.
main :: IO ()
main = hspec spec

--------------------------------

data Foo
data Bar

spec :: Spec
spec = do
    describe "Many" $ do

        -- it "Test user friendly compile errors" $ do
        --     let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
        --     -- ghc 8.0.2: IndexOf error: ‘Maybe Bool’ is not a member of ...
        --     -- ghc 8.0.1 has terrible error message: "No instance for NatToInt"
        --     grab @(Maybe Bool) y `shouldBe` (Just False)

        --     -- Not unique error: ‘Maybe Char’ is a duplicate in ...
        --     grab @(Maybe Bool) y `shouldBe` (Just False)

        it "is a Typeable" $ do
            let x = (5 :: Int) ./ False ./ nil
                y = cast x :: Maybe (Many '[Int, String])
                z = cast x :: Maybe (Many '[Int, Bool])
            y `shouldBe` Nothing
            z `shouldBe` Just x
#if __GLASGOW_HASKELL__ >= 802
            let expected = "Many (': * Int (': * Bool ('[] *)))"
#else
            let expected = "Many (': * Int (': * Bool '[]))"
#endif
            (show . typeRep . (pure @Proxy) $ x) `shouldBe` expected

        it "is a Read and Show" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nil"
                s' = "5 ./ False ./ 'X' ./ (Just 'O' ./ (nil))"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                x' = read s' :: Many '[Int, Bool, Char, Maybe Char]
            show x `shouldBe` s
            show x' `shouldBe` s

        it "is a Eq" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nil"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                y = 5 ./ False ./ 'X' ./ Just 'O' ./ nil
            x `shouldBe` y

        it "is an Ord" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nil"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                y5o = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                y4o = (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                y5p = (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil
            compare x y5o `shouldBe` EQ
            compare y4o y5o `shouldBe` LT
            compare y5o y4o `shouldBe` GT
            compare y5o y5p `shouldBe` LT
            compare y5p y5o `shouldBe` GT

        it "can converted to and from a tuple" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                t = ((5 :: Int), False, 'X', Just 'O')
            x `shouldBe` toMany' t
            t `shouldBe` fromMany' x

        it "can construct using 'single', 'nil', 'consMany', 'snocMany', 'append'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                x' = (5 :: Int) `consMany` False `consMany` 'X' `consMany` Just 'O' `consMany` nil
                y = single (5 :: Int) \. False \. 'X' \. Just 'O'
                y' = single (5 :: Int) `snocMany` False `snocMany` 'X' `snocMany` Just 'O'
                a = single (5 :: Int) `snocMany` False
                b = single 'X' `snocMany` Just 'O'
            x `shouldBe` y
            x `shouldBe` x'
            y `shouldBe` y'
            a /./ b `shouldBe` x
            a `append` b `shouldBe` x

        it "can 'snocMany'' a value only if that type doesn't already exist" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                y = x `snocMany'` True
            y `shouldBe` x

        -- it "can 'append'' the unique types from another Many" $ do
        --     let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
        --         y = (5 :: Int) ./ Just True ./ 'X' ./ Just False ./ Just (6 :: Int) ./ Just 'O' ./ nil
        --     (x `append'` y) `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ Just True ./ Just (6 :: Int) ./ nil

        it "can contain multiple fields of the same type" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x /./ (6 :: Int) ./ Just 'A' ./ nil) `shouldBe` y

        it "can destruct using 'front', 'back', 'aft', 'fore'" $ do
            let a = (x ./ y) \. z
                x = 5 :: Int
                y = single False ./ 'X' ./ nil
                z = Just 'O'
            front a `shouldBe` x
            back a `shouldBe` z
            aft a `shouldBe` (y \. z)
            fore a `shouldBe` x ./ y

        it "has getter for unique fields using 'grab'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            grab @Int x `shouldBe` 5
            grab @Bool x `shouldBe` False
            grab @Char x `shouldBe` 'X'
            grab @(Maybe Char) x `shouldBe` Just 'O'

        it "has getter for for unique fields using 'grabN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            grabN @0 x `shouldBe` 5
            grabN @1 x `shouldBe` False
            grabN @2 x `shouldBe` 'X'
            grabN @3 x `shouldBe` Just 'O'

        it "has getter for duplicate fields using 'grabN'" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            grabN @0 y `shouldBe` 5
            grabN @1 y `shouldBe` False
            grabN @2 y `shouldBe` 'X'
            grabN @3 y `shouldBe` Just 'O'
            grabN @4 y `shouldBe` 6
            grabN @5 y `shouldBe` Just 'A'

        it "with duplicate fields can still use 'grab' for unique fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            grab @Bool x `shouldBe` False
            grab @Char x `shouldBe` 'X'

        it "has getter for unique labels using 'grabL'" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Tagged @"Hello" (6 :: Int) ./ nil
            grab @(Tagged Foo _) y `shouldBe` Tagged @Foo 'X'
            grabL @Foo y `shouldBe` Tagged @Foo 'X'
            grabL @"Hello" y `shouldBe` Tagged @"Hello" (6 :: Int)

        it "has setter for unique fields using 'replace''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replace' @Int x 6 `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replace' x True `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nil
            replace' x 'O' `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ nil
            replace' x (Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil

        it "has polymorphic setter for unique fields using 'replace'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replace @Int x 'Z' `shouldBe` 'Z' ./ False ./ 'X' ./ Just 'O' ./ nil
            replace @Bool x 'Z' `shouldBe` (5 :: Int) ./ 'Z' ./ 'X' ./ Just 'O' ./ nil
            replace @(Maybe Char) x 'Z' `shouldBe` (5 :: Int) ./ False ./ 'X' ./ 'Z' ./ nil

        it "has setter for unique labels using 'replaceL''" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replace' @(Tagged Foo _) y (Tagged @Foo 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Foo 'Y' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replaceL' @Foo y (Tagged @Foo 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Foo 'Y' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replaceL' @"Hello" y (Tagged @"Hello" 7) `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Tagged @"Hello" (7 :: Int) ./ nil

        it "has polymorphic setter for unique labels using 'replaceL'" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replace @(Tagged Foo Char) y (Tagged @Bar 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Bar 'Y' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replaceL @Foo y (Tagged @Bar 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Bar 'Y' ./ Tagged @"Hello" (6 :: Int) ./ nil
            replaceL @"Hello" y (Tagged @"Hello" False) `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Tagged @"Hello" False ./ nil

        it "has setter for unique fields using 'replaceN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replaceN' @0 x (7 :: Int) `shouldBe`
                (7 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replaceN' @1 x True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nil
            replaceN' @2 x 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ nil
            replaceN' @3 x (Just 'P') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil

        it "has polymorphic setter using 'replaceN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            replaceN @0 x True `shouldBe`
                True ./ False ./ 'X' ./ Just 'O' ./ nil
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN @1 y 'Y' `shouldBe`
                (5 :: Int) ./ 'Y' ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN @5 y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ 'Y' ./ nil

        it "has setter for duplicate fields using 'replaceN''" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN' @0 y (7 :: Int) `shouldBe`
                (7 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN' @1 y True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN' @2 y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN' @3 y (Just 'P') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ (6 :: Int) ./ Just 'A' ./ nil
            replaceN' @4 y (8 :: Int) `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'A' ./ nil
            replaceN' @5 y (Just 'B') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'B' ./ nil

        it "has setter for unique fields using 'replace'' (even if there are other duplicate fields)" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replace' @Bool y True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            replace' @Char y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil

        it "has getter for multiple fields using 'select'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            select @'[Int, Maybe Char] x `shouldBe` (5 :: Int) ./ Just 'O' ./ nil

        it "has getter for multiple labelled fields using 'selectL'" $ do
            let x = False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'O' ./ nil
            selectL @'[Foo, Bar] x `shouldBe` Tagged @Foo False ./ Tagged @Bar 'X' ./ nil
            selectL @'["Hi", "Bye"] x `shouldBe` Tagged @"Hi" (5 :: Int) ./ Tagged @"Bye" 'O' ./ nil
            -- below won't compile because the type of labels must match
            -- selectL @'["Hi", 'Foo, "Bye"] x `shouldBe` Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @"Bye" 'O' ./ nil

        it "can reorder fields using 'select' or 'selectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            select @'[Bool, Int, Maybe Char] x `shouldBe` False ./ (5 :: Int) ./ Just 'O' ./ nil
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            selectN @'[5, 4, 0, 1, 3, 2] y `shouldBe`
                Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ False ./ Just 'O' ./ 'X' ./ nil

        it "has getter for multiple fields with duplicates using 'selectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            selectN @'[5, 4, 0] x `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nil

        it "can't select into types from indistinct fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            -- Compile error: Int is a duplicate
            -- select @[Bool, Char, Int] x `shouldBe` False ./ 'X' ./ (5 :: Int) ./ nil
            x `shouldBe` x

        it "with duplicate fields has getter for multiple unique fields 'select'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            select @'[Bool, Char] x `shouldBe` False ./ 'X' ./ nil

        it "has setter for multiple fields using 'amend''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            amend' @'[Int, Maybe Char] x ((6 :: Int) ./ Just 'P' ./ nil) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil

        it "has polymorphc setter for multiple fields using 'amend'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            amend @'[Int, Maybe Char] x ("Foo" ./ "Bar" ./ nil) `shouldBe` "Foo" ./ False ./ 'X' ./ "Bar" ./ nil

        it "has setter for multiple labelled fields using 'amendL''" $ do
            let x = False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'O' ./ nil
            amendL' @'[Foo, Bar] x (Tagged @Foo True ./ Tagged @Bar 'Y' ./ nil) `shouldBe`
                False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo True ./ Tagged @Bar 'Y' ./ Tagged @"Bye" 'O' ./ nil
            amendL' @'["Hi", "Bye"] x (Tagged @"Hi" (6 :: Int) ./ Tagged @"Bye" 'P' ./ nil) `shouldBe`
                False ./ Tagged @"Hi" (6 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'P' ./ nil

        it "has polymorphic setter for multiple labelled fields using 'amendL'" $ do
            let x = False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'O' ./ nil
            amendL @'[Foo, Bar] x ('Y' ./ True ./ nil) `shouldBe`
                False ./ Tagged @"Hi" (5 :: Int) ./ 'Y' ./ True ./ Tagged @"Bye" 'O' ./ nil
            amendL @'["Hi", "Bye"] x (True ./ Tagged @"Changed" True ./ nil) `shouldBe`
                False ./ True ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Changed" True ./ nil

        it "has setter for multiple fields with duplicates using 'amendN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            amendN' @'[5, 4, 0] x (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nil) `shouldBe`
                (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nil

        it "has polymorphic setter for multiple fields with duplicates using 'amendN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            amendN @'[5, 4, 0] x ("Foo" ./ Just 'B' ./ 'Z' ./ nil) `shouldBe`
                'Z' ./ False ./ 'X' ./ Just 'O' ./ Just 'B' ./ "Foo" ./ nil

        it "can't amend into types from indistinct fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            -- Compile error: Int is a duplicate
            -- amend' @ '[Bool, Char, Int] x (True ./ 'B' ./ (8 :: Int) ./ nil) `shouldBe`
            --     (5 :: Int) ./ True ./ 'B' ./ Just 'O' ./ (8 :: Int) ./ Just 'A' ./ nil
            x `shouldBe` x

        it "with duplicate fields has setter for unique fields 'amend''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            amend' @'[Bool, Char] x (True ./ 'B' ./ nil) `shouldBe`
                (5 :: Int) ./ True ./ 'B' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil

        it "can be folded with 'Many' handlers using 'forMany' or 'collect'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
                y = show @Int ./ show @Char ./ show @(Maybe Char) ./ show @Bool ./ nil
                ret = ["5", "False", "'X'", "Just 'O'", "6", "Just 'A'"]
            afoldr (:) [] (collect x (cases y)) `shouldBe` ret
            afoldr (:) [] (forMany (cases y) x) `shouldBe` ret
            afoldr (:) [] (forMany (cases y) x) `shouldBe` ret

        it "can be folded with polymorphic 'CaseFunc' handlers using 'forMany' or 'collect'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            afoldr (:) [] (forMany (CaseFunc @Typeable (show . typeRep . (pure @Proxy))) x) `shouldBe` ["Int", "Bool", "Char", "Maybe Char", "Int", "Maybe Char"]

        it "can be folded with 'Many' handlers in index order using 'forManyN' or 'collectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
                y = show @Int ./ show @Bool ./ show @Char ./ show @(Maybe Char) ./ show @Int ./ show @(Maybe Char) ./ nil
                ret = ["5", "False", "'X'", "Just 'O'", "6", "Just 'A'"]
            afoldr (:) [] (collectN x (casesN y)) `shouldBe` ret
            afoldr (:) [] (forManyN (casesN y) x) `shouldBe` ret

        it "every hasLens can be mapped into a different type in a Functor-like fashion with using 'afmap'" $ do
            let x = (5 :: Int) ./ (6 :: Int8) ./ (7 :: Int16) ./ (8 :: Int32) ./ nil
                y = (15 :: Int) ./ (16 :: Int8) ./ (17 :: Int16) ./ (18 :: Int32) ./ nil
                z = ("5" :: String) ./ ("6" :: String) ./ ("7" :: String) ./ ("8" :: String) ./ nil
                mx = (Just 5 :: Maybe Int) ./ ([6] :: [Int8]) ./ nil
                my = (Just 15 :: Maybe Int) ./ ([16] :: [Int8]) ./ nil
                mz = (Just "5" :: Maybe String) ./ (["6"] :: [String]) ./ nil
            afmap (CaseFunc' @Num (+10)) x `shouldBe` y
            afmap (CaseFunc @Show @String show) x `shouldBe` z
            afmap (CaseFunc1' @C0 @Functor @Num (fmap (+10))) mx `shouldBe` my
            afmap (CaseFunc1 @C0 @Functor @(C2 Show Read) @String (fmap show)) mx `shouldBe` mz
