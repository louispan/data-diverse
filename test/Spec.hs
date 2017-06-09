{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens
import Data.Distinct.Catalog
import Data.Distinct.Many.Internal
import Data.Typeable
import Test.Hspec

-- | get type of a value
proxy :: a -> Proxy a
proxy _ = Proxy

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
                (x ^. item @Int, x ^. item @Bool) `shouldBe` (5, False)
            it "can be projected" $ do
                let x = catalog (5, False) :: Catalog '[Int, Bool]
                    y = catalog 5
                y `shouldBe` (x ^. project @(Catalog '[Int]))
            it "is a Typeable" $ do
                let x = catalog (5, False) :: Catalog '[Int, Bool]
                    y = cast x :: Maybe (Catalog '[Int, String])
                    z = cast x :: Maybe (Catalog '[Int, Bool])
                y `shouldBe` Nothing
                z `shouldBe` Just x

        describe "Many" $ do
            it "can be constructed and destructed" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                    x = preview (facet @Int) y
                x `shouldBe` (Just 5)
            it "can be switched with a catalog of handlers in any order" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (cases
                    ( show @Bool
                    , show @Int)
                    ) `shouldBe` "5"
            it "can be switched with AnyCase" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y ((AnyCase (show . typeRep . proxy)) :: AnyCase String) `shouldBe` "Int"

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
