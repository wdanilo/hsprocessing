{-# LANGUAGE DataKinds #-}

module Math.Space.Metric.SDF where

import Prologue

import Data.Array.Linear (BVec)
import Math.Space.Dimension




-- === SDF ===

newtype SDF dim a = SDF { runSDF :: BVec dim a -> a }

type IsSDF t a = Convertible (t a) (SDF (DimOf t) a)

type instance DimOf (SDF dim) = dim

instance Show (SDF dim a) where show _ = "SDF"
