module Graphics.Display.Object where

import Prologue

import qualified Math.Algebra.Boolean      as Bool
import           Data.Array.Linear
import           Math.Space.Dimension
import           Graphics.Shading.Material
import qualified Graphics.Shading.Flat     as Flat


-- === Display object ===

newtype Object     l t a = Object (Shaded l (Transformed t) a) deriving (Show, Functor, Traversable, Foldable)
type    Composite  (o :: (* -> *) -> * -> *) t   = o (Bool.Expred t)

-- instances

type instance MaterialOf (Object l t a) = Material l
type instance DimOf      (Object l t)   = DimOf t

instance HasMaterial     (Object l t a) where material = wrapped . material

instance Rewrapped (Object l t a) (Object l' t' a')
instance Wrapped   (Object l t a) where
    type Unwrapped (Object l t a) = Shaded l (Transformed t) a
    _Wrapped' = iso (\(Object a) -> a) Object
