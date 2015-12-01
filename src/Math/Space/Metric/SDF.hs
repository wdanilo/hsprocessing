{-# LANGUAGE DataKinds #-}

module Math.Space.Metric.SDF where

import Prologue

import Data.Array.Linear (BVec)
import Math.Space.Dimension
import Math.Algebra.Boolean hiding (Expr)
import           Language.GLSL.Syntax



-- === SDF ===

newtype SDF dim a = SDF { runSDF :: BVec dim a -> a }

type IsSDF t a = Convertible (t a) (SDF (DimOf (t a)) a)

type instance DimOf (SDF dim a) = dim

instance Show (SDF dim a) where show _ = "SDF"






data Combination a = Combination       a
                   | Diffused    Float a
                   deriving (Show, Functor)


-- data Item (dim :: Nat) a = Simple (SDF dim Expr)
--                          | Boolean a
