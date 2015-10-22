{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.Array.Linear.Properties where

import Prologue


type family ShapeOf (a :: k) :: [Nat]
type SizeOf a = SingletonEl (ShapeOf a)


type family SingletonEl (a :: k) :: l
type instance SingletonEl '[a] = a


