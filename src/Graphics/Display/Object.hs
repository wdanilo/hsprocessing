module Graphics.Display.Object where

import Prologue

import           Data.Array.Linear
import           Graphics.Shading.Material
import qualified Graphics.Shading.Flat     as Flat
import qualified Math.Algebra.Boolean      as Bool
import           Math.Space.Dimension


-- === Display object ===

newtype Object     l t a = Object (Shaded l (Transformed t) a) deriving (Show, Functor, Traversable, Foldable)
type    Compound   l t   = Object l (Bool.Compound t)

-- instances

type instance MaterialOf (Object l t a) = Material l
type instance DimOf      (Object l t a) = DimOf (t a)

instance HasMaterial     (Object l t a) where material = wrapped . material

instance Rewrapped (Object l t a) (Object l' t' a')
instance Wrapped   (Object l t a) where
    type Unwrapped (Object l t a) = Shaded l (Transformed t) a
    _Wrapped' = iso (\(Object a) -> a) Object
