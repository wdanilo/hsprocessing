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

object :: SDF 2 Expr -> Object 2
object = Object . Display.Object . Shaded def . Transformed mempty . Compound . Bool.Val



-- === External instances ===


instance (Convertible a Expr, KnownNat dim) => Convertible (BVec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v