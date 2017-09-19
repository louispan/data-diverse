{-# LANGUAGE FlexibleContexts #-}

module Data.Diverse.Profunctor where

import Data.Diverse.Many
import Data.Diverse.Which
import Data.Diverse.TypeLevel

-- | Like 'Strong' but using 'Many'
class Itemized p where
    itemized
        :: UniqueMember a as
        => p a b -> p (Many as) (Many (Replace a b as))

-- | Like 'Choice' but using 'Which'
class Faceted p where
    faceted
        :: ( UniqueMember a as
           , UniqueMember b bs
           , Diversify bs (Without a as))
        => p a b -> p (Which as) (Which bs)

-- | Like 'Itemized' but transforming from 'Many'
class Projected p where
    projected
        :: (Select as as', Amend' as bs as')
        => p (Many as) (Many bs) -> p (Many as') (Many (Replaces as bs as'))

-- | Like 'Faceted' but transforming from 'Which'
class Injected p where
    injected
        :: ( Reinterpret as as'
           , Diversify bs' (Complement as' as)
           , Diversify bs' bs)
        => p (Which as) (Which bs) -> p (Which as') (Which bs')
