{-# LANGUAGE NoMonomorphismRestriction #-}

module Graphics.Rendering.GLSL.SDF where


import Prologue

import           Data.Array.Linear
import           Language.GLSL.Syntax      (Expr)
import           Math.Algebra.Boolean      (Compound(..))
import qualified Math.Algebra.Boolean      as Bool
import           Math.Space.Dimension      (DimOf)
import           Math.Space.Metric.SDF     (SDF)
import           Graphics.Shading.Material (Shaded(..))
import qualified Graphics.Display.Object   as Display
import           Graphics.Shading.Flat     (Layer)
import           Graphics.Shading.Material (MaterialOf, Material, HasMaterial, material)
import           Language.GLSL.DSL         ()


-- === Types ===

newtype Object (dim :: Nat) = Object (Unwrapped (Object dim)) deriving (Show)

instance Rewrapped (Object dim) (Object dim')
instance Wrapped   (Object dim) where
	type Unwrapped (Object dim) = Display.Object (Layer Expr) (Compound (SDF dim)) Expr
	_Wrapped' = iso (\(Object a) -> a) Object

-- instances

type instance DimOf      (Object dim) = dim
type instance MaterialOf (Object dim) = Material (Layer Expr)

instance HasMaterial (Object dim) where material = wrapped . material

-- utils

object :: SDF n Expr -> Object n
object = Object . Display.Object . Shaded def . Transformed mempty . Compound . Bool.Val

translate :: Vector 3 Boxed Expr -> Object n -> Object n
translate v (Object (Display.Object (Shaded material (Transformed _ comp)))) =
    (Object (Display.Object (Shaded material (Transformed xform comp)))) where
        xform = translation v :: BQuaternion Expr


-- === External instances ===

unwrap'     = view wrapped'   -- update to Prologue >= 1.0.6 and remove

instance (Convertible a Expr, KnownNat dim) => Convertible (BVec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v

instance (Convertible a Expr, KnownNat dim) => Convertible (Matrix dim dim Boxed a) Expr where
    convert v = fromString ("mat" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v

instance (Convertible a Expr) => Convertible (BQuaternion a) Expr where
    convert = convert . unwrap'
