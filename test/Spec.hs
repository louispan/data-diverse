{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Distinct.Catalog
import Data.Distinct.Many.Internal
import Control.Lens
import Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "Catalog" $ do
            it "is a Read and Show" $ do
                let s = "Catalog2 (5,False)"
                    x = read s :: Catalog '[Int, Bool]
                show x `shouldBe` s
            it "is a Eq" $ do
                let s = "Catalog2 (5,False)"
                    x = read s :: Catalog '[Int, Bool]
                    y = catalog (5, False)
                x `shouldBe` y
            it "can edit fields" $ do
                let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
                    y = x & item @Int .~ 6
                    z = catalog (6, False)
                y `shouldBe` z
            it "can read both fields" $ do
                let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
                    y = x ^. item @Int
                (x ^. item @Int, x ^. item @Bool) `shouldBe` (5, False)
            it "can be projected" $ do
                let x = catalog (5, False) :: Catalog '[Int, Bool]
                    y = catalog 5
                y `shouldBe` (x ^. project @(Catalog '[Int]))

        describe "Many" $ do
            it "can be constructed and destructedh" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                    x = preview (facet @Int) y
                x `shouldBe` (Just 5)
            it "can be switched" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                -- switch y (catalog
                --          (show, show) :: Catalog '[Int -> String, Bool -> String]) `shouldBe` "5"
                -- • Couldn't match expected type ‘Unwrapped (Catalog s0)’
                --   with actual type ‘(Int -> String, Bool -> String)’
                --   The type variable ‘s0’ is ambiguous
                switch y (catalog
                         (show :: Int -> String, show :: Bool -> String)) `shouldBe` "5"






            -- it "can be switched with AnyCase" $ do
            --     let y = pick (5 :: Int) :: Many '[Int, Bool]
            --     switch y ((AnyCase show) :: AnyCase String) `shouldBe` "5"
        --     it "is a Read and Show" $ do
        --         let s = "M2_1 5"
        --             x = read s :: Many '[Int, Bool]
        --         show x `shouldBe` s
        --     it "is a Eq" $ do
        --         let s = "M2_1 5"
        --             x = read s :: Many '[Int, Bool]
        --             y = M2_1 5
        --         x `shouldBe` y
        -- describe "Many" $ do
