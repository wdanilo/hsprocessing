module Math.Space.Metric.Bounded where

import Prologue          hiding (Bounded)
import Data.Array.Linear (BVec)
import Math.Space.Dimension (Dim, DimOf)



-- Types

data Bounded b t a = Bounded (BVec (DimOf t) b) (t a) deriving (Show)
type Bounded' t a = Bounded a t a

type family BoundsOf t

class HasBounds t where
    bounds :: Lens' t (BoundsOf t) 


-- Utils

bounded :: Lens' (Bounded b t a) (t a)
bounded = lens (\(Bounded b a) -> a) (\(Bounded b _) a -> Bounded b a)


-- Instances

type instance DimOf    (Bounded b t)   = DimOf t
type instance BoundsOf (Bounded b t a) = BVec (DimOf t) b

instance HasBounds (Bounded b t a) where
    bounds = lens (\(Bounded b _) -> b) (\(Bounded _ t) b -> Bounded b t)

