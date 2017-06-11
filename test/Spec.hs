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
import Data.Distinct
import Data.Typeable
import Test.Hspec

-- import GHC.TypeLits
-- import Data.Proxy
-- import Data.Maybe
-- import Data.Distinct.Many.Internal
-- import Unsafe.Coerce

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

            it "can be sampled until it's not many" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                    -- y' = pick (5 :: Int) :: Many '[Int]
                    x = trialEither y :: Either (Many '[Int]) Bool
                -- x `shouldBe` (Left y') -- FIXME: Eq not done yet
                case x of
                    Left y' -> (notMany y') `shouldBe` (5 :: Int)
                    Right _ -> pure ()

            it "can be switched with a catalog of handlers in any order" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (cases
                    ( show @Bool
                    , show @Int
                    )) `shouldBe` "5"

            it "can be switched with a catalog of handlers with extraneous content" $ do
                let y = pick (5 :: Int) :: Many '[Int]
                switch y (Cases (catalog( -- cannot use `cases` function if you can redundant cases
                    ( show @Int
                    , 20 :: Int
                    )))) `shouldBe` "5"

            it "can be switched with TypeableCase" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (TypeableCase @'[Int, Bool] (show . typeRep . proxy)) `shouldBe` "Int"

            it "can be switched with TypeableCase unambiguously" $ do
                let y = pick (5 :: Int) :: Many '[Int, Bool]
                switch y (TypeableCase (show . typeRep . proxy)) `shouldBe` "Int"

            -- it "can be switched with forany" $ do
            --     let y = sample (5 :: Int) :: Many '[Int, Bool]
            --     let x = case y of
            --                 Many n v -> let someNat = fromJust (someNatVal (toInteger n))
            --                             in case someNat of
            --                                    SomeNat (_ :: Proxy x) ->
            --                                        (show . typeRep . proxy) (unsafeCoerce v :: TypeAt x '[Int, Bool])

            --     x `shouldBe` "Int"

            -- it "can be switched with forany" $ do
            --     let y = sample (5 :: Int) :: Many '[Int, Bool]
            --     forany y (show . typeRep . proxy) `shouldBe` "Int"

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
