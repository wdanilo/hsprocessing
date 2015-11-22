{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Graphics.Rendering.GLSL.SDF.Figures where

import Prologue

import           Graphics.Rendering.GLSL.SDF    (Object, object)
import           Language.GLSL.Syntax           (Expr)
import           Math.Space.Dimension           (Dim(Dim), convertInDim)
import           Math.Space.Metric.SDF          (SDF(SDF))
import qualified Math.Topology.Geometry.Figures as F
import qualified Data.Array.Linear              as L
import           GHC.TypeLits

--ball = F.ball

--myBall :: Bounded Float (Object 2)
--myBall = Bounded (A.vec2 400 400) (object $ (convert $ (ball (100.0 :: GLSL.Expr) :: Dim 2 Ball GLSL.Expr) :: SDF 2 GLSL.Expr))
--       & material .~ mtl


ball :: Convertible (Dim dim F.Ball Expr) (SDF dim Expr) => Expr -> Object dim
ball r = object $ convertInDim $ F.ball r

hyperrectangle :: Convertible (F.Hyperrectangle dim t Expr) (SDF dim Expr) => L.Vector dim t Expr -> Object dim
hyperrectangle s = object $ convertInDim $ F.hyperrectangle s

hyperrectangleRounded :: Convertible (F.HyperrectangleRounded dim t Expr) (SDF dim Expr) => L.Vector dim t Expr -> L.Vector (2 ^ dim) t Expr -> Object dim
hyperrectangleRounded s c = object $ convertInDim $ F.hyperrectangleRounded s c

halfspace :: Convertible (F.Halfspace dim t Expr) (SDF dim Expr) => L.Vector dim t Expr -> Object dim
halfspace a = object $ convertInDim $ F.halfspace a




instance (Convertible a Expr, n ~ 2) => Convertible (Dim 2 F.Ball a) (SDF n Expr) where
    convert (Dim (F.Ball r)) = SDF $ \v -> "sdf_ball" [convert v, convert r]

instance (Convertible (L.Vector 2 t a) Expr, Convertible a Expr, n ~ 2) =>
        Convertible (F.Hyperrectangle 2 t a) (SDF n Expr) where
    convert (F.Hyperrectangle s) = SDF $ \v -> "sdf_rect" [convert v, convert s]

instance (Convertible (L.Vector 2 t a) Expr, Convertible (L.Vector 4 t a) Expr, Convertible a Expr, n ~ 2) =>
        Convertible (F.HyperrectangleRounded 2 t a) (SDF n Expr) where
    convert (F.HyperrectangleRounded s c) = SDF $ \v -> "sdf_rect" [convert v, convert s, convert c]

instance (Convertible (L.Vector 2 t a) Expr, Convertible a Expr, n ~ 2) =>
        Convertible (F.Halfspace 2 t a) (SDF n Expr) where
    convert (F.Halfspace a) = SDF $ \v -> "sdf_halfplane" [convert v, convert a]
