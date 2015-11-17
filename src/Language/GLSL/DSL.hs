{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GLSL.DSL where

import           Prelude                        ()
import           Prologue                       hiding (div, (.>), (.=), void)
import           Language.GLSL                  ()
import           Language.GLSL.Syntax
import           Text.PrettyPrint.HughesPJClass (prettyShow, Pretty)
import           Data.String                    (IsString, fromString)
import qualified Data.Char as Char
import           Data.Convert
import           GHC.TypeLits                   (Nat)
import qualified Control.Monad.State           as State
import           GHC.TypeLits
import           Control.Monad.State           (State, runState, evalState, execState, get, put)
import           Data.Array.Linear.Color.Class
import           Data.Array.Linear.Color.Modes
import           Data.Array.Linear.Color.Attrs.RGBA

--import qualified Data.Array.Repa as Repa
--import           Data.Array.Repa hiding (Shape)
--import qualified Data.Array.Repa.Matrix as Repa
--import qualified Data.Array.Repa.Repr.Unboxed as Repa

--import           Data.Vector.Unboxed (Unbox)
--import           Data.Array.Repa.Matrix (Matrix, Quaternion)
--import qualified Data.Array.Repa.Matrix as Mx

import qualified Data.Vector as V

--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Array.Repa.Repr.Vector as Repa

import qualified Data.Array.Linear as A

import Data.RTuple

unit :: [ExternalDeclaration] -> TranslationUnit
unit = TranslationUnit

typeSpec t = TypeSpec Nothing (TypeSpecNoPrecision t Nothing)

void  = typeSpec Void
float = typeSpec Float
vec2  = typeSpec Vec2
vec3  = typeSpec Vec3
vec4  = typeSpec Vec4
mat4  = typeSpec Mat4

param t = ParameterDeclaration Nothing Nothing t Nothing

var = Variable

sub = Sub
mul = Mul
add = Add
div = Div

app name params = FunctionCall (FuncId name) (Params params)


func tp name args body = FunctionDefinition (FuncProt (FullType Nothing tp) name args) (Compound body)
func' = func void

val tp (Variable name) expr = DeclarationStatement (InitDeclaration (TypeDeclarator (FullType Nothing tp))  [ InitDecl name Nothing (Just expr) ])


a .> fields = FieldSelection a fields

assignment a b = ExpressionStatement (Just (Equal a b))

(.=) = assignment


--class Assi
--val

instance IsString Expr where fromString = var


instance Fractional Expr where
    fromRational = FloatConstant . fromRational
    (/) = div

instance Num Expr where
    fromInteger = IntConstant Decimal . fromInteger
    (*) = mul
    (+) = add
    (-) = sub

instance Convertible Float Expr where
    convert = FloatConstant

-- instance Convertible (Vector n t a) Expr where

--instance IsString TypeSpecifier where
    --fromString (s:ss) = typeSpec $ Char.toUpper s : ss
    --(+), (*), abs, signum, fromInteger, (negate | (-))


instance t ~ Expr => IsString ([t] -> Expr) where fromString = app

uniformDecl :: String -> ExternalDeclaration
uniformDecl name = Declaration
                 $ InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Uniform)) (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))))
                   [ InitDecl name Nothing $ Just (FloatConstant 1.0) ]

uniformDecl2 :: String -> ExternalDeclaration
uniformDecl2 name = Declaration
                  $ InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Uniform)) (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))))
                    [ InitDecl name Nothing Nothing ]

--data Geometry pos  = Geometry pos

--data Shape = Shape


--data family V (dim :: Nat) a
--data instance V 1 a = V1 a     deriving (Show)
--data instance V 2 a = V2 a a   deriving (Show)
--data instance V 3 a = V3 a a a deriving (Show)



--xxx :: _ => _
--xxx = V2 1 2


--type Circle a = Geom 2 (Spherical a)

--data Circle a = Circle a

--circle :: Float -> Circle Float
--circle = Circle

--data Rect   a = Rect   a a


--data SDF = forall s. IsSDF s => SDF [s]

--data family SDF (dim :: Nat) a

--data instance SDF 2 a = SDF2 (Expr -> Expr)
--data instance SDF 2 a = SDF2 Expr deriving (Show)

--data Colored a = Colored Color a
--type SDF = Expr -> Expr

--class Bounded

--class IsSDF (dim :: Nat) s a where
--    sdf :: (s a) -> SDF dim a

--instance Convertible a Expr => IsSDF 2 Ball a where
--    sdf (Ball r) = SDF2 $ \p -> "sdf_circle" [p, convert r]


---- ===

--type family DimensionsOf a :: Choose Nat

---- ===

--type family Replicate (num :: Nat) a :: [*] where Replicate 0 a = '[]
--                                                  Replicate n a = (a ': Replicate (n-1) a)


--data Choose a = Only a
--              | OneOf [a]
--              | Any
--              deriving (Show)



---- == Coordinate spaces ===

--data Coord_Space coord space dim a = Coord_Space coord (space dim a)

--type Coord_Space_Homogeneous   = Coord_Space Homogeneous
--type Coord_Space_Cartesian     = Coord_Space Cartesian
--type Coord_Space_Spherical     = Coord_Space Spherical
--type Coord_Space_Cylindrical s = Coord_Space Cylindrical s 3


---- === Coordinate systems ===


--data Homogeneous = Homogeneous
--data Cartesian   = Cartesian
--data Spherical   = Spherical
--data Cylindrical = Cylindrical



---- === Metrics ===

--data Discrete  = Discrete
--data Euclidean = Euclidean
--data Taxicab   = Taxicab

--data Metric_Space metric dim a = Metric_Space metric (Space dim a) deriving (Show)

--type Metric_Space_Discrete  = Metric_Space Discrete
--type Metric_Space_Euclidean = Metric_Space Euclidean
--type Metric_Space_Taxicab   = Metric_Space Euclidean


---- === Vector Spaces ===



----- === Spaces ===

--data Space dim a = Space


--type Space_R dim = Space dim Rational
--type Space_I dim = Space dim Integer
--type Space_F dim = Space dim Float



--Metric_Space Euclidean 2 Float


