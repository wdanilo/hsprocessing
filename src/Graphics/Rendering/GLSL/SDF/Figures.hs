{-# LANGUAGE OverloadedStrings #-}

module Graphics.Rendering.GLSL.SDF.Figures where

import Prologue

import           Graphics.Rendering.GLSL.SDF    (Object, object)
import           Language.GLSL.Syntax           (Expr)
import           Math.Space.Dimension           (Dim(Dim), convertInDim)
import           Math.Space.Metric.SDF          (SDF(SDF))
import           Math.Topology.Geometry.Figures (Ball(Ball), Hyperrectangle(Hyperrectangle))
import qualified Math.Topology.Geometry.Figures as F

--ball = F.ball

--ball = F.ball


--myBall :: Bounded Float (Object 2)
--myBall = Bounded (A.vec2 400 400) (object $ (convert $ (ball (100.0 :: GLSL.Expr) :: Dim 2 Ball GLSL.Expr) :: SDF 2 GLSL.Expr))
--       & material .~ mtl


ball :: Convertible (Dim dim F.Ball Expr) (SDF dim Expr) => Expr -> Object dim
ball r = object $ convertInDim $ F.ball r

hyperrectangle :: Convertible (Dim dim F.Hyperrectangle Expr) (SDF dim Expr) => Expr -> Object dim
hyperrectangle s = object $ convertInDim $ F.hyperrectangle s






instance (Convertible a Expr, n ~ 2) => Convertible (Dim 2 Ball a) (SDF n Expr) where
    convert (Dim (Ball r)) = SDF $ \v -> "sdf_ball" [convert v, convert r]

instance (Convertible a Expr, n ~ 2) => Convertible (Dim 2 Hyperrectangle a) (SDF n Expr) where
    convert (Dim (Hyperrectangle s)) = SDF $ \v -> "sdf_rect" [convert v, convert s]
