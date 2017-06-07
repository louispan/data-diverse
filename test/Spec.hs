{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Distinct.Catalog
import Control.Lens

main :: IO ()
main = do
    print wock'''
    let r = read "C2 (5, False)" :: Catalog '[Int, Bool]
    print r

-- -- instance Has b (Catalog '[a, b]) where
-- --     get (R2 (a, b)) = b
wock :: Catalog '[Int, String]
wock = review _Wrapped (5, "hi")

wock' :: Catalog '[Int, Catalog '[Int, String]]
wock' = review _Wrapped (5, wock)

-- wock'' :: Catalog '[Int, Int]
-- wock'' = review _Wrapped (5, 6)

wock''' :: Catalog '[Int, String]
wock''' = review _Cataloged' (5 :: Int, "6" :: String)


-- wock'' :: Catalog '[Int, Int]
-- wock'' = catalog (5, 6)

wack :: (Int, String)
wack = (view (item @Int) wock', view (item @(Catalog '[Int, String]) . item) wock')
