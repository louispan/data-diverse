{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Data.Diverse
import Data.Typeable
import Test.Hspec

import GHC.TypeLits

-- | get type of a value
proxy :: a -> Proxy a
proxy _ = Proxy

main :: IO ()
main = do
    hspec $ do
        describe "Nary" $ do
            it "is a Typeable" $ do
                let x = (5 :: Int) ./ False ./ nul
                    y = cast x :: Maybe (Nary '[Int, String])
                    z = cast x :: Maybe (Nary '[Int, Bool])
                y `shouldBe` Nothing
                z `shouldBe` Just x

            it "is a Read and Show" $ do
                let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                    x = read s :: Nary '[Int, Bool, Char, Maybe Char]
                show x `shouldBe` s

            it "is a Eq" $ do
                let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                    x = read s :: Nary '[Int, Bool, Char, Maybe Char]
                    y = 5 ./ False ./ 'X' ./ Just 'O' ./ nul
                x `shouldBe` y

            it "is an Ord" $ do
                let s = "5 ./ False ./ 'X' ./ Just 'O' ./ nul"
                    x = read s :: Nary '[Int, Bool, Char, Maybe Char]
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
                    y' = ((5 :: Int), False, 'X', Just 'O')
                    y = toNary' y'
                    x' = fromNary' x
                x `shouldBe` y
                x' `shouldBe` y'

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

            it "has getter using 'fetch'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                fetch @Int x `shouldBe` 5
                x .^. (Proxy @Int) `shouldBe` 5
                fetch @Bool x `shouldBe` False
                x .^. (Proxy @Bool) `shouldBe` False
                fetch @Char x `shouldBe` 'X'
                x .^. (Proxy @Char) `shouldBe` 'X'
                fetch @(Maybe Char) x `shouldBe` Just 'O'
                x .^. (Proxy @(Maybe Char))`shouldBe` Just 'O'

            it "has getter for duplicate fields using 'fetchN'" $ do
                let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                fetchN @0 Proxy y `shouldBe` 5
                fetchN @1 Proxy y `shouldBe` False
                fetchN @2 Proxy y `shouldBe` 'X'
                fetchN @3 Proxy y `shouldBe` Just 'O'
                fetchN @4 Proxy y `shouldBe` 6
                fetchN @5 Proxy y `shouldBe` Just 'A'

            it "has setter using 'replace'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                replace @Int x 6 `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                (x .~. (6 :: Int)) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                replace x True `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
                (x .~. True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
                replace x 'O' `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ nul
                (x .~. 'O') `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ nul
                replace x (Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul
                (x .~. (Just 'P')) `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

            it "has setter for duplicate fields using 'replaceN'" $ do
                let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                replaceN @0 Proxy y 7 `shouldBe` (7 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                replaceN @1 Proxy y True `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                replaceN @2 Proxy y 'Y' `shouldBe` (5 :: Int) ./ False ./ 'Y' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                replaceN @3 Proxy y (Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ (6 :: Int) ./ Just 'A' ./ nul
                replaceN @4 Proxy y 8 `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'A' ./ nul
                replaceN @5 Proxy y (Just 'B') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'B' ./ nul

            it "has getter/setter lens using 'item'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                x ^. item @Int `shouldBe` 5
                (x & item @Int .~ 6) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                x ^. item @Bool `shouldBe` False
                (x & item @Bool .~ True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nul
                x ^. item @Char `shouldBe` 'X'
                (x & item @Char .~ 'O') `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ nul
                x ^. item @(Maybe Char) `shouldBe` Just 'O'
                (x & item @(Maybe Char) .~ Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

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

            it "has getter for multiple fields using 'narrow'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                narrow @'[Int, Maybe Char] x `shouldBe` (5 :: Int) ./ Just 'O' ./ nul
                x \^. (Proxy @'[Int, Maybe Char]) `shouldBe` (5 :: Int) ./ Just 'O' ./ nul

            it "can reorder fields using 'narrow' or 'narrowN'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                narrow @'[Bool, Int, Maybe Char] x `shouldBe` False ./ (5 :: Int) ./ Just 'O' ./ nul
                let y = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                narrowN (Proxy @'[5, 4, 0, 1, 3, 2]) y `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ False ./ Just 'O' ./ 'X' ./ nul

            it "has getter for multiple fields with duplicates using 'narrowN'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                narrowN (Proxy @'[5, 4, 0]) x `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nul

            it "has setter for multiple fields using 'amend'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                amend @'[Int, Maybe Char] x ((6 :: Int) ./ Just 'P' ./ nul) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul
                (x \~. (6 :: Int) ./ Just 'P' ./ nul) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

            it "has setter for multiple fields with duplicates using 'amendN'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                amendN (Proxy @'[5, 4, 0]) x (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nul) `shouldBe`
                    (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nul

            it "has getter/setter lens for multiple fields using 'project'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nul
                x ^. (project @'[Int, Maybe Char]) `shouldBe` (5 :: Int) ./ Just 'O' ./ nul
                (x & (project @'[Int, Maybe Char]) .~ ((6 :: Int) ./ Just 'P' ./ nul)) `shouldBe`
                    (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nul

            it "has getter/setter lens for multiple fields with duplicates using 'projectN'" $ do
                let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nul
                x ^. (projectN (Proxy @'[5, 4, 0])) `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nul
                (x & (projectN (Proxy @'[5, 4, 0])) .~ (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nul)) `shouldBe`
                    (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nul

        describe "TypeLevel" $ do
            it "PositionOf" $ do
                fromIntegral (natVal @(PositionOf String '[Bool, Int]) Proxy) `shouldBe` (0 :: Int)
                fromIntegral (natVal @(PositionOf Bool '[Bool, Int]) Proxy) `shouldBe` (1 :: Int)
                fromIntegral (natVal @(PositionOf Int '[Bool, Int]) Proxy) `shouldBe` (2 :: Int)
                fromIntegral (natVal @(PositionOf Int '[Bool, Int, Char]) Proxy) `shouldBe` (2 :: Int)
                fromIntegral (natVal @(PositionOf Int '[Bool, String, Char]) Proxy) `shouldBe` (0 :: Int)

            it "ComplementOf" $ do
                let complementTest :: (Complement xs ys ~ comp) => Proxy xs -> Proxy ys -> Proxy comp -> Proxy comp
                    complementTest _ _ comp = comp

                complementTest (Proxy @[String, Int]) (Proxy @[Bool, Int]) (Proxy @'[String]) `shouldBe` Proxy
                complementTest (Proxy @[String, Int, Char]) (Proxy @[Bool, Int]) (Proxy @[String, Char]) `shouldBe` Proxy
                complementTest (Proxy @[Bool, Int]) (Proxy @[Bool, Int]) (Proxy @'[]) `shouldBe` Proxy
                complementTest (Proxy @[String, Bool]) (Proxy @[Int, Char]) (Proxy @'[String, Bool]) `shouldBe` Proxy

        describe "Many" $ do
            it "can be constructed and destructed" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                    x = preview (facet @Int) y
                x `shouldBe` (Just 5)

            it "can be trialled until it's not a Many" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                    y' = pick (5 :: Int) :: Many '[Int]
                    x = trial y :: Either (Many '[Int]) Bool
                x `shouldBe` (Left y')

            it "can be switched with a Nary of handlers in any order" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (cases
                    (  show @Bool
                    ./ show @Int
                    ./ nul)) `shouldBe` "5"

            it "can be switched with a Nary of handlers with extraneous content" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                switch y (Cases ( -- contrast with 'cases' which disallows extraneous content
                    ( show @Int
                    ./ 'X'
                    ./ False
                    ./ nul
                    ))) `shouldBe` "5"

            it "can be switched with CaseTypeable" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (CaseTypeable @'[Int, Bool] (show . typeRep . proxy)) `shouldBe` "Int"

            it "can be switched with CaseTypeable unambiguously" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (CaseTypeable (show . typeRep . proxy)) `shouldBe` "Int"

            it "can be extended and rearranged with diversify" $ do
                let y = pick' (5 :: Int)
                    y' = diversify @[Int, Bool] y
                    y'' = diversify @[Bool, Int] y'
                switch y'' (CaseTypeable (show . typeRep . proxy)) `shouldBe` "Int"

            it "can be reinterpreted into a different Many" $ do
                let y = pick @[Int, Char] (5 :: Int)
                    y' = (reinterpret @[String, Bool] y)
                y' `shouldBe` Left y

            it "can be reinterpreted into either one of two different Many" $ do
                let y = pick @[Int, Char] (5 :: Int)
                    y' = reinterpret @[String, Bool] y
                y' `shouldBe` Left y

            it "can be reinterpreted into either one of two different Many" $ do
                let y = pick @[Int, Char] (5 :: Int)
                    y' = reinterpret @[String, Char] y
                y' `shouldBe` Left (pick (5 :: Int))

            it "can be reinterpreted into either one of two different Many" $ do
                let y = pick @[Int, Char] (5 :: Int)
                    y' = reinterpret @[String, Int] y
                y' `shouldBe` Right (pick (5 :: Int))

            it "is a Read and Show" $ do
                let s = "Many 5"
                    x = read s :: Many '[Int, Bool]
                show x `shouldBe` "Many 5"

            it "is a Eq" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                let y' = pick (5 :: Int) :: Many '[Int, Bool]
                y `shouldBe` y'
