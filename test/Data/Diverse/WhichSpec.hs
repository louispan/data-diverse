{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.WhichSpec (main, spec) where

import Control.Lens
import Data.Diverse
import Data.Typeable
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- | get type of a value
proxy :: a -> Proxy a
proxy _ = Proxy

-- | Utility to convert Either to Maybe
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

spec :: Spec
spec = do
    describe "Which" $ do

        it "is a Read and Show" $ do
            let s = "pick 5"
                x = read s :: Which '[Int, Bool]
            show x `shouldBe` "pick 5"
            "impossible" `shouldBe` show impossible
            "impossible" `shouldBe` show (read "impossible" :: Which '[])

        it "is an Eq" $ do
            let y = pick (5 :: Int) :: Which '[Int, Bool]
            let y' = pick (5 :: Int) :: Which '[Int, Bool]
            y `shouldBe` y'
            read (show impossible) `shouldBe` impossible

        it "is an Ord" $ do
            let y5 = pick (5 :: Int) :: Which '[Int, Bool]
            let y6 = pick (6 :: Int) :: Which '[Int, Bool]
            compare y5 y5 `shouldBe` EQ
            compare y5 y6 `shouldBe` LT
            compare y6 y5 `shouldBe` GT
            compare impossible impossible `shouldBe` EQ

        it "can be constructed by type with 'pick' and destructed with 'trial'" $ do
            let y = pick (5 :: Int) :: Which '[Bool, Int, Char]
                x = hush $ trial @Int y
            x `shouldBe` (Just 5)

        it "may contain possiblities of duplicate types" $ do
            let y = pick (5 :: Int) :: Which '[Bool, Int, Char, Bool, Char]
                x = hush $ trial @Int y
            x `shouldBe` (Just 5)

        it "can be constructed conveniently with 'pick'' and destructed with 'trial'" $ do
            let y = pick' (5 :: Int)
                x = hush $ trial' y
            x `shouldBe` (Just 5)

        it "can be constructed by index with 'pickN' and destructed with 'trialN" $ do
            let y = pickN (Proxy @4) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
                x = hush $ trialN (Proxy @4) y
            x `shouldBe` (Just 5)

        it "can be 'trial'led until its final 'conclude' value" $ do
            let x = pick (5 :: Int) :: Which '[Int, Bool]
                y = pick (5 :: Int) :: Which '[Int]
                x' = trial x :: Either (Which '[Int]) Bool
            x' `shouldBe` (Left y)
            conclude y `shouldBe` 5

        it "can be constructed and destructed by type with 'facet'" $ do
            let y = review (facet @Int) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Char]
                x = preview (facet @Int) y
            x `shouldBe` (Just 5)

        it "can be constructed and destructed by index with 'facetN'" $ do
            let y = review (facetN (Proxy @4)) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
                x = preview (facetN (Proxy @4)) y
            x `shouldBe` (Just 5)

        it "can be extended and rearranged by type with 'diversify'" $ do
            let y = pick' (5 :: Int)
                y' = diversify @[Int, Bool] y
                y'' = diversify @[Bool, Int] y'
            switch y'' (CaseTypeable (show . typeRep . proxy)) `shouldBe` "Int"

        it "can be extended and rearranged by index with 'diversify'" $ do
            let y = pick' (5 :: Int)
                y' = diversifyN @'[0] @[Int, Bool] Proxy y
                y'' = diversifyN @[1,0] @[Bool, Int] Proxy y'
            switch y'' (CaseTypeable (show . typeRep . proxy)) `shouldBe` "Int"

        it "can be 'reinterpret'ed by type into a totally different Which" $ do
            let y = pick @[Int, Char] (5 :: Int)
                a = reinterpret @[String, Bool] y
            a `shouldBe` Left y
            let  b = reinterpret @[String, Char] y
            b `shouldBe` Left (pick (5 :: Int))
            let c = reinterpret @[String, Int] y
            c `shouldBe` Right (pick (5 :: Int))

        it "can be 'reinterpretN'ed by index into a subset Which" $ do
            let y = pick @[Char, String, Int, Bool] (5 :: Int)
                a = reinterpretN' @[2, 0] @[Int, Char] Proxy y
                a' = reinterpretN' @[3, 0] @[Bool, Char] Proxy y
            a `shouldBe` Just (pick (5 :: Int))
            a' `shouldBe` Nothing

        it "can be 'diversify'ed and 'reinterpreted' by type with 'inject'" $ do
            let x = pick (5 :: Int) :: Which '[String, Int]
                y = review (inject @_ @[Bool, Int, Char, String]) x
            y `shouldBe` pick (5 :: Int)
            let y' = preview (inject @[String, Int]) y
            y' `shouldBe` Just (pick (5 :: Int))

        it "can be 'diversifyN'ed and 'reinterpretedN' by index with 'injectN'" $ do
            let x = pick (5 :: Int) :: Which '[String, Int]
                y = review (injectN @[3, 1] @_ @[Bool, Int, Char, String] Proxy) x
            y `shouldBe` pick (5 :: Int)
            let y' = preview (injectN @[3, 1] @[String, Int] Proxy) y
            y' `shouldBe` Just (pick (5 :: Int))

        it "can be 'switch'ed with a Many of handlers in any order" $ do
            let y = pickN @0 Proxy (5 :: Int) :: Which '[Int, Bool, Bool, Int]
            switch y (
                cases (show @Bool
                    ./ show @Int
                    ./ nul)) `shouldBe` "5"

        it "can be 'switch'ed with a Many of handlers with extraneous content" $ do
            let y = pick (5 :: Int) :: Which '[Int, Bool]
            switch y (
                -- contrast with lowercase 'cases' which disallows extraneous content
                Cases (show @Int
                    ./ show @Bool
                    ./ show @Char
                    ./ 'X'
                    ./ False
                    ./ nul
                )) `shouldBe` "5"

        it "can be 'switchN'ed with a Many of handlers in index order" $ do
            let y = pickN @0 Proxy (5 :: Int) :: Which '[Int, Bool, Bool, Int]
            switchN y (
                casesN (show @Int
                    ./ show @Bool
                    ./ show @Bool
                    ./ show @Int
                    ./ nul)) `shouldBe` "5"

        it "can be switched with CaseTypeable" $ do
            let y = pick (5 :: Int) :: Which '[Int, Bool]
            switch y (CaseTypeable (show . typeRep . proxy)) `shouldBe` "Int"
            (show . typeRep . proxy $ y) `shouldBe` "Which (': * Int (': * Bool '[]))"
