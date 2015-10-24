module Graphics.Shading.Material where

import Prologue                  hiding (Bounded)

import Math.Space.Metric.Bounded (Bounded)


-- === Materials ===

data Material l     = Material [l]              deriving (Show, Functor, Traversable, Foldable)
data Shaded   l t a = Shaded (Material l) (t a) deriving (Show, Functor, Traversable, Foldable)

type family MaterialOf a

class HasMaterial t where
    material :: Lens' t (MaterialOf t)

--

instance Default (Material a) where def = Material mempty

type instance MaterialOf  (Shaded l t a) = Material l
instance      HasMaterial (Shaded l t a) where
   material = lens (\(Shaded m _) -> m) (\(Shaded _ t) m -> Shaded m t)

---------

type instance MaterialOf (Bounded b t a) = MaterialOf (t a)