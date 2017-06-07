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
                let s = "(5,False)"
                    x = read s :: Catalog '[Int, Bool]
                show x `shouldBe` s
            it "is a Eq" $ do
                let s = "(5,False)"
                    x = read s :: Catalog '[Int, Bool]
                    y = review _Cataloged (5, False)
                x `shouldBe` y
            it "can edit fields" $ do
                let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
                    y = x & item @Int .~ 6
                    z = review _Cataloged (6, False)
                y `shouldBe` z
            it "can be projected" $ do
                let x = review _Cataloged (5, False) :: Catalog '[Int, Bool]
                    y = review _Cataloged 5
                y `shouldBe` (x ^. project @(Catalog '[Int]))

        describe "Many" $ do
            it "is a Read and Show" $ do
                let s = "M2_1 5"
                    x = read s :: Many '[Int, Bool]
                show x `shouldBe` s
            it "is a Eq" $ do
                let s = "M2_1 5"
                    x = read s :: Many '[Int, Bool]
                    y = M2_1 5
                x `shouldBe` y
