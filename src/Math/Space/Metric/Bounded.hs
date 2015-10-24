module Math.Space.Metric.Bounded where

import Prologue          hiding (Bounded)
import Data.Array.Linear (BVec)
import Math.Space.Dimension (Dim, DimOf)



-- Types

data Bounded b t = Bounded (BVec (DimOf t) b) t deriving (Show)
--type Bounded'  t = Bounded (ElementOf t)

type family BoundsOf t
type family Content  t

class Bounds t where
    bounds  :: Lens' t (BoundsOf t) 
    bounded :: Lens' t (Content  t)


-- Instances

type instance DimOf    (Bounded b t) = DimOf t
type instance BoundsOf (Bounded b t) = BVec (DimOf t) b
type instance Content  (Bounded b t) = t

instance Bounds (Bounded b t) where
    bounds = lens (\(Bounded b _) -> b) (\(Bounded _ t) b -> Bounded b t)
    bounded = lens (\(Bounded b t) -> t) (\(Bounded b _) t -> Bounded b t)

