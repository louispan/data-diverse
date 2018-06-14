{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.TypeSpec (main, spec) where

import Data.Diverse
import Data.Typeable
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TypeLevel" $ do
        it "PositionOf" $ do
            natToInt @(PositionOf String '[Bool, Int]) `shouldBe` (0 :: Int)
            natToInt @(PositionOf Bool '[Bool, Int]) `shouldBe` (1 :: Int)
            natToInt @(PositionOf Int '[Bool, Int]) `shouldBe` (2 :: Int)
            natToInt @(PositionOf Int '[Bool, Int, Char]) `shouldBe` (2 :: Int)
            natToInt @(PositionOf Int '[Bool, String, Char]) `shouldBe` (0 :: Int)

        it "ComplementOf" $ do
            let complementTest :: Proxy xs -> Proxy ys -> Proxy (Complement xs ys) -> Proxy (Complement xs ys)
                complementTest _ _ comp = comp

            complementTest (Proxy @[String, Int]) (Proxy @[Bool, Int]) (Proxy @'[String]) `shouldBe` Proxy
            complementTest (Proxy @[String, Int, Char]) (Proxy @[Bool, Int]) (Proxy @[String, Char]) `shouldBe` Proxy
            complementTest (Proxy @[Bool, Int]) (Proxy @[Bool, Int]) (Proxy @'[]) `shouldBe` Proxy
            complementTest (Proxy @[String, Bool]) (Proxy @[Int, Char]) (Proxy @'[String, Bool]) `shouldBe` Proxy
