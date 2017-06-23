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
import GHC.TypeLits
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
