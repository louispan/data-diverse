{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens
import Data.Diverse
import Data.Diverse.Type
import Data.Diverse.CaseTypeable
import Data.Typeable
import Test.Hspec

import GHC.TypeLits
-- import Data.Proxy
-- import Data.Maybe
-- import Data.Distinct.Many.Internal
-- import Unsafe.Coerce

-- | get type of a value
proxy :: a -> Proxy a
proxy _ = Proxy

-- -- | Utility to convert Either to Maybe
-- hush :: Either a b -> Maybe b
-- hush = either (const Nothing) Just

main :: IO ()
main = do
    hspec $ do
        describe "Nary" $ do
            it "is a Read and Show" $ do
                let s = "5 ./ False ./ nul"
                    x = read s :: Nary '[Int, Bool]
                show x `shouldBe` s

            it "is a Eq" $ do
                let s = "5 ./ False ./ nul"
                    x = read s :: Nary '[Int, Bool]
                    y = 5 ./ False ./ nul
                x `shouldBe` y

        --     it "can edit fields" $ do
        --         let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
        --             y = x & item @Int .~ 6
        --             z = catalog (6, False)
        --         y `shouldBe` z

        --     it "can read both fields" $ do
        --         let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
        --         (x ^. item @Int, x ^. item @Bool) `shouldBe` (5, False)

        --     it "can be projected" $ do
        --         let x = catalog (5, False) :: Catalog '[Int, Bool]
        --             y = catalog 5
        --         y `shouldBe` (x ^. project @(Catalog '[Int]))

        --     it "is a Typeable" $ do
        --         let x = catalog (5, False) :: Catalog '[Int, Bool]
        --             y = cast x :: Maybe (Catalog '[Int, String])
        --             z = cast x :: Maybe (Catalog '[Int, Bool])
        --         y `shouldBe` Nothing
        --         z `shouldBe` Just x

        -- describe "TypeLevel" $ do
        --     it "PositionOf" $ do
        --         fromIntegral (natVal @(PositionOf String '[Bool, Int]) Proxy) `shouldBe` (0 :: Int)
        --         fromIntegral (natVal @(PositionOf Bool '[Bool, Int]) Proxy) `shouldBe` (1 :: Int)
        --         fromIntegral (natVal @(PositionOf Int '[Bool, Int]) Proxy) `shouldBe` (2 :: Int)
        --         fromIntegral (natVal @(PositionOf Int '[Bool, Int, Char]) Proxy) `shouldBe` (2 :: Int)
        --         fromIntegral (natVal @(PositionOf Int '[Bool, String, Char]) Proxy) `shouldBe` (0 :: Int)

        --     it "ComplementOf" $ do
        --         let complementTest :: (Complement xs ys ~ comp) => Proxy xs -> Proxy ys -> Proxy comp -> Proxy comp
        --             complementTest _ _ comp = comp

        --         complementTest (Proxy @[String, Int]) (Proxy @[Bool, Int]) (Proxy @'[String]) `shouldBe` Proxy
        --         complementTest (Proxy @[String, Int, Char]) (Proxy @[Bool, Int]) (Proxy @[String, Char]) `shouldBe` Proxy
        --         complementTest (Proxy @[Bool, Int]) (Proxy @[Bool, Int]) (Proxy @'[]) `shouldBe` Proxy
        --         complementTest (Proxy @[String, Bool]) (Proxy @[Int, Char]) (Proxy @'[String, Bool]) `shouldBe` Proxy

        describe "Many" $ do
            it "can be constructed and destructed" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                    x = preview (facet @Int) y
                x `shouldBe` (Just 5)

            it "can be trialled until it's not a Many" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                    -- y' = pick (5 :: Int) :: Many '[Int]
                    x = trial y :: Either (Many '[Int]) Bool
                -- x `shouldBe` (Left y') -- FIXME: Eq not done yet
                case x of
                    Left y' -> (notMany y') `shouldBe` (5 :: Int)
                    Right _ -> pure ()

            -- it "can be switched with a catalog of handlers in any order" $ do
            --     let y = pick (5 :: Int) :: Many '[Int, Bool]
            --     switch y (cases
            --         ( show @Bool
            --         , show @Int
            --         )) `shouldBe` "5"

            -- it "can be switched with a catalog of handlers with extraneous content" $ do
            --     let y = pick (5 :: Int) :: Many '[Int]
            --     switch y (Cases (catalog( -- cannot use `cases` function if you can redundant cases
            --         ( show @Int
            --         , 20 :: Int
            --         -- , False
            --         )))) `shouldBe` "5"

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
