{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Diverse.Class.Project where

import Control.Lens
import Data.Kind

-- | Projection.
-- A Catalog can be narrowed or have its order changed by projecting into another Catalog type.
-- Use TypeApplication to specify the @to@ destination typelist of the lens.
-- Example: @project \@(Catalog '[Int, String])@
class Project (to :: [Type]) (from :: [Type]) t where
    project :: Lens' (t from) (t to)

-- | This is 'project' with the type parameters reversed
-- so TypeApplications can be used to specify @from@ instead of @to@.
-- Example: @projected \@(Catalog '[Int, String])@
projected :: forall from to t. Project to from t => Lens' (t from) (t to)
projected = project
{-# INLINE projected #-}
