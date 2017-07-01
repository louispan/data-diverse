{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.ManySpec (main, spec) where

import Control.Lens
import Data.Diverse
import Data.Typeable
import Data.Tagged
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

data Foo
data Bar

spec :: Spec
spec = do
    describe "Many" $ do
        it "is a Typeable" $ do
            let x = (5 :: Int) ./ False ./ nul
                y = cast x :: Maybe (Many '[Int, String])
                z = cast x :: Maybe (Many '[Int, Bool])
            y `shouldBe` Nothing
            z `shouldBe` Just x
            (show . typeRep . (pure @Proxy) $ x) `shouldBe` "Many (': * Int (': * Bool '[]))"

        it "is a Read and Show" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                s' = "5 ./ False ./ 'X' ./ (Just 'O' ./ (nul))"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                x' = read s' :: Many '[Int, Bool, Char, Maybe Char]
            show x `shouldBe` s
            show x' `shouldBe` s

        it "is a Eq" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                y = 5 ./ False ./ 'X' ./ Just 'O' ./ nul
            x `shouldBe` y

        it "is an Ord" $ do
            let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                x = read s :: Many '[Int, Bool, Char, Maybe Char]
                y5o = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                y4o = (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                y5p = (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul
            compare x y5o `shouldBe` EQ
            compare y4o y5o `shouldBe` LT
            compare y5o y4o `shouldBe` GT
            compare y5o y5p `shouldBe` LT
            compare y5p y5o `shouldBe` GT

        it "can converted to and from a tuple" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                t = ((5 :: Int), False, 'X', Just 'O')
            x `shouldBe` toMany' t
            x `shouldBe` review _Many' t
            t `shouldBe` fromMany' x
            t `shouldBe` view _Many' x

        it "can construct using 'single', 'nul', 'prefix', 'postfix', 'append'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                x' = (5 :: Int) `prefix` False `prefix` 'X' `prefix` Just 'O' `prefix` nul
                y = single (5 :: Int) \. False \. 'X' \. Just 'O'
                y' = single (5 :: Int) `postfix` False `postfix` 'X' `postfix` Just 'O'
                a = single (5 :: Int) `postfix` False
                b = single 'X' `postfix` Just 'O'
            x `shouldBe` y
            x `shouldBe` x'
            y `shouldBe` y'
            a /./ b `shouldBe` x
            a `append` b `shouldBe` x

        it "can contain multiple fields of the same type" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x /./ (6 :: Int) ./ Just 'A' ./ nul) `shouldBe` y

        it "can destruct using 'front', 'back', 'aft', 'fore'" $ do
            let a = (x ./ y) \. z
                x = 5 :: Int
                y = single False ./ 'X' ./ nul
                z = Just 'O'
            front a `shouldBe` x
            back a `shouldBe` z
            aft a `shouldBe` (y \. z)
            fore a `shouldBe` x ./ y

        it "has getter for unique fields using 'fetch'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            fetch @Int x `shouldBe` 5
            fetch @Bool x `shouldBe` False
            fetch @Char x `shouldBe` 'X'
            fetch @(Maybe Char) x `shouldBe` Just 'O'

        it "has getter for for unique fields using 'fetchN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            fetchN @0 Proxy x `shouldBe` 5
            fetchN @1 Proxy x `shouldBe` False
            fetchN @2 Proxy x `shouldBe` 'X'
            fetchN @3 Proxy x `shouldBe` Just 'O'

        it "has getter for duplicate fields using 'fetchN'" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            fetchN @0 Proxy y `shouldBe` 5
            fetchN @1 Proxy y `shouldBe` False
            fetchN @2 Proxy y `shouldBe` 'X'
            fetchN @3 Proxy y `shouldBe` Just 'O'
            fetchN @4 Proxy y `shouldBe` 6
            fetchN @5 Proxy y `shouldBe` Just 'A'

        it "with duplicate fields can still use 'fetch' for unique fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            fetch @Bool x `shouldBe` False
            fetch @Char x `shouldBe` 'X'

        it "can 'fetch' usng tagged labels" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            fetch @(Tagged Foo _) y `shouldBe` Tagged @Foo 'X'

        it "has setter for unique fields using 'replace'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replace @Int x 6 `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replace x True `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
            replace x 'O' `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ nul
            replace x (Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

        it "has polymorphic setter for unique fields using 'replace'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replace' @Int Proxy x 'Z' `shouldBe` 'Z' ./ False ./ 'X' ./ Just 'O' ./ nul
            replace' @Bool Proxy x 'Z' `shouldBe` (5 :: Int) ./ 'Z' ./ 'X' ./ Just 'O' ./ nul
            replace' @(Maybe Char) Proxy x 'Z' `shouldBe` (5 :: Int) ./ False ./ 'X' ./ 'Z' ./ nul

        it "has setter for unique fields using 'replaceN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replaceN @0 Proxy x (7 :: Int) `shouldBe`
                (7 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replaceN @1 Proxy x True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
            replaceN @2 Proxy x 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ nul
            replaceN @3 Proxy x (Just 'P') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

        it "has polymorphic setter using 'replaceN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            replaceN' @0 Proxy x True `shouldBe`
                True ./ False ./ 'X' ./ Just 'O' ./ nul
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN' @1 Proxy y 'Y' `shouldBe`
                (5 :: Int) ./ 'Y' ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN' @5 Proxy y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ 'Y' ./ nul

        it "has setter for duplicate fields using 'replaceN'" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN @0 Proxy y (7 :: Int) `shouldBe`
                (7 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN @1 Proxy y True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN @2 Proxy y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN @3 Proxy y (Just 'P') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ (6 :: Int) ./ Just 'A' ./ nul
            replaceN @4 Proxy y (8 :: Int) `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'A' ./ nul
            replaceN @5 Proxy y (Just 'B') `shouldBe`
                (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'B' ./ nul

        it "has setter for unique fields using 'replace' (even if there are other duplicate fields)" $ do
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replace @Bool y True `shouldBe`
                (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replace @Char y 'Y' `shouldBe`
                (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul

        it "can 'replace' usng tagged labels" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replace @(Tagged Foo _) y (Tagged @Foo 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Foo 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul

        it "can 'replace'' polymorphically usng tagged labels" $ do
            let y = (5 :: Int) ./ False ./ Tagged @Foo 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            replace' @(Tagged Foo Char) Proxy y (Tagged @Bar 'Y') `shouldBe`
                (5 :: Int) ./ False ./ Tagged @Bar 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul

        it "has getter/setter lens using 'item'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            x ^. item @Int `shouldBe` 5
            (x & item @Int .~ 6) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            x ^. item @Bool `shouldBe` False
            (x & item @Bool .~ True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
            x ^. item @Char `shouldBe` 'X'
            x ^. item @(Maybe Char) `shouldBe` Just 'O'

        it "has polymorphic getter/setter lens using 'item''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            (x & item' @(Maybe Char) .~ Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul
            (x & item' @Int .~ 'Z') `shouldBe` 'Z' ./ False ./ 'X' ./ Just 'O' ./ nul
            (x & item' @Bool .~ 'Z') `shouldBe` (5 :: Int) ./ 'Z' ./ 'X' ./ Just 'O' ./ nul
            (x & item' @Char .~ True) `shouldBe` (5 :: Int) ./ False ./ True ./ Just 'O' ./ nul
            (x & item' @(Maybe Char) .~ 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ 'P' ./ nul

        it "has getter/setter lens for duplicate fields using 'itemN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @0) `shouldBe` 5
            (x & itemN (Proxy @0) .~ 6) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @1) `shouldBe` False
            (x & itemN (Proxy @1) .~ True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @2) `shouldBe` 'X'
            (x & itemN (Proxy @2) .~ 'O') `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @3) `shouldBe` Just 'O'
            (x & itemN (Proxy @3) .~ Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @4) `shouldBe` 6
            (x & itemN (Proxy @4) .~ 7) `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (7 :: Int) ./ Just 'A' ./ nul
            x ^. itemN (Proxy @5) `shouldBe` Just 'A'
            (x & itemN (Proxy @5) .~ Just 'B') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'B' ./ nul

        it "has polymorphic getter/setter lens for duplicate fields using 'itemN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & itemN' (Proxy @0) .~ "Foo") `shouldBe` "Foo" ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & itemN' (Proxy @1) .~ "Foo") `shouldBe` (5 :: Int) ./ "Foo" ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & itemN' (Proxy @2) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ "Foo" ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & itemN' (Proxy @3) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ "Foo" ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & itemN' (Proxy @4) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ "Foo" ./ Just 'A' ./ nul
            (x & itemN' (Proxy @5) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ "Foo" ./ nul

        it "has getter for multiple fields using 'select'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            select @'[Int, Maybe Char] x `shouldBe` (5 :: Int) ./ Just 'O' ./ nul

        it "can reorder fields using 'select' or 'selectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            select @'[Bool, Int, Maybe Char] x `shouldBe` False ./ (5 :: Int) ./ Just 'O' ./ nul
            let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            selectN (Proxy @'[5, 4, 0, 1, 3, 2]) y `shouldBe`
                Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ False ./ Just 'O' ./ 'X' ./ nul

        it "has getter for multiple fields with duplicates using 'selectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            selectN (Proxy @'[5, 4, 0]) x `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nul

        it "can't select into types from indistinct fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            -- Compile error: Int is a duplicate
            -- select @[Bool, Char, Int] x `shouldBe` False ./ 'X' ./ (5 :: Int) ./ nul
            x `shouldBe`  x

        it "with duplicate fields has getter for multiple unique fields 'select'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            select @'[Bool, Char] x `shouldBe` False ./ 'X' ./ nul

        it "has setter for multiple fields using 'amend'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            amend @'[Int, Maybe Char] x ((6 :: Int) ./ Just 'P' ./ nul) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

        it "has polymorphc setter for multiple fields using 'amend'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            amend' @'[Int, Maybe Char] Proxy x ("Foo" ./ "Bar" ./ nul) `shouldBe` "Foo" ./ False ./ 'X' ./ "Bar" ./ nul

        it "has setter for multiple fields with duplicates using 'amendN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            amendN (Proxy @'[5, 4, 0]) x (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nul) `shouldBe`
                (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nul

        it "has polymorphic setter for multiple fields with duplicates using 'amendN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            amendN' @'[5, 4, 0] Proxy x ("Foo" ./ Just 'B' ./ 'Z' ./ nul) `shouldBe`
                'Z' ./ False ./ 'X' ./ Just 'O' ./ Just 'B' ./ "Foo" ./ nul

        it "can't amend into types from indistinct fields" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            -- Compile error: Int is a duplicate
            -- amend @ '[Bool, Char, Int] x (True ./ 'B' ./ (8 :: Int) ./ nul) `shouldBe`
            --     (5 :: Int) ./ True ./ 'B' ./ Just 'O' ./ (8 :: Int) ./ Just 'A' ./ nul
            x `shouldBe` x

        it "with duplicate fields has setter for unique fields 'amend'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            amend @ '[Bool, Char] x (True ./ 'B' ./ nul) `shouldBe`
                (5 :: Int) ./ True ./ 'B' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul

        it "has getter/setter lens for multiple fields using 'project'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            x ^. (project @'[Int, Maybe Char]) `shouldBe` (5 :: Int) ./ Just 'O' ./ nul
            (x & (project @'[Int, Maybe Char]) .~ ((6 :: Int) ./ Just 'P' ./ nul)) `shouldBe`
                (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

        it "has polymorphic getter/setter lens for multiple fields using 'project'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
            (x & (project' @'[Int, Maybe Char]) .~ ("Foo" ./ Just "Bar" ./ nul)) `shouldBe`
                "Foo" ./ False ./ 'X' ./ Just "Bar" ./ nul

        it "has getter/setter lens for multiple fields with duplicates using 'projectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            x ^. (projectN @'[5, 4, 0] Proxy) `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nul
            (x & (projectN @'[5, 4, 0] Proxy) .~ (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nul)) `shouldBe`
                (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nul

        it "has polymorphic getter/setter lens for multiple fields with duplicates using 'projectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            (x & (projectN' @'[5, 4, 0] Proxy) .~ (Just "Foo" ./ (8 :: Int) ./ "Bar" ./ nul)) `shouldBe`
                "Bar" ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just "Foo" ./ nul

        it "can be folded with 'Many' handlers using 'forMany' or 'collect'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                y = show @Int ./ show @Char ./ show @(Maybe Char) ./ show @Bool ./ nul
                ret = ["5", "False", "'X'", "Just 'O'", "6", "Just 'A'"]
            afoldr (:) [] (collect x (cases y)) `shouldBe` ret
            afoldr (:) [] (forMany (cases y) x) `shouldBe` ret
            afoldr (:) [] (forMany (cases y) x) `shouldBe` ret

        it "can be folded with single 'CaseTypeable' handlers using 'forMany' or 'collect'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
            afoldr (:) [] (forMany (CaseTypeable (show . typeRep . (pure @Proxy))) x) `shouldBe` ["Int", "Bool", "Char", "Maybe Char", "Int", "Maybe Char"]

        it "can be folded with 'Many' handlers in index order using 'forManyN' or 'collectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                y = show @Int ./ show @Bool ./ show @Char ./ show @(Maybe Char) ./ show @Int ./ show @(Maybe Char) ./ nul
                ret = ["5", "False", "'X'", "Just 'O'", "6", "Just 'A'"]
            afoldr (:) [] (collectN x (casesN y)) `shouldBe` ret
            afoldr (:) [] (forManyN (casesN y) x) `shouldBe` ret
