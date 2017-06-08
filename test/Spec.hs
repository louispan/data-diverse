{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Distinct.Catalog
import Data.Distinct.Many
import Control.Lens
import Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "Catalog" $ do
            it "is a Read and Show" $ do
                let s = "C2 (5,False)"
                    x = read s :: Catalog '[Int, Bool]
                show x `shouldBe` s
            it "is a Eq" $ do
                let s = "C2 (5,False)"
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

        -- describe "Many" $ do
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
