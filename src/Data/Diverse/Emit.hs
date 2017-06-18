{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Data.Diverse.Emit where

class Emit g (xs :: [k]) r where
    emit :: g xs r -> r
