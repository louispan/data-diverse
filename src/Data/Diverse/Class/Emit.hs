{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.Class.Emit where

import Data.Kind

class Emit g (xs :: [Type]) r where
    emit :: g xs r -> r
