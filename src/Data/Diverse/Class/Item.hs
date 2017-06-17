{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Diverse.Class.Item where

import Control.Lens

-- | A catalog has a lens to an item.
class Item value record where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @item \@Int@
    item :: Lens' record value
