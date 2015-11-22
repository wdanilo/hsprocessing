{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}


module Math.Topology.Geometry.Figures where


import Prologue
import Math.Space.Dimension (Dim, DimOf, embed')
import Data.Array.Linear
import GHC.TypeLits


newtype Ball a = Ball a deriving (Show, Functor, Foldable, Traversable)

type Circle a = Dim 2 Ball a


newtype Hyperrectangle (dim :: Nat) t a = Hyperrectangle (Vector dim t a)

type instance DimOf (Hyperrectangle dim t a) = dim

type Rectangle t a = Hyperrectangle 2 t a


data HyperrectangleRounded (dim :: Nat) t a = HyperrectangleRounded (Vector dim t a) (Vector (2 ^ dim) t a)

type instance DimOf (HyperrectangleRounded dim t a) = dim

type RectangleRounded t a = HyperrectangleRounded 2 t a


data Halfspace (dim :: Nat) t a = Halfspace (Vector dim t a)

type instance DimOf (Halfspace dim t a) = dim

type Halfplane t a = Halfspace 2 t a


-- TODO: Extend the definition to Regular prism (https://en.wikipedia.org/wiki/Cube)
-- data Rect a = Rect (Vector 2 a) deriving (Show)


ball :: a -> Dim n Ball a
ball = embed' . Ball

circle :: a -> Circle a
circle = ball


hyperrectangle :: Vector dim t a -> Hyperrectangle dim t a
hyperrectangle = Hyperrectangle

rectangle :: Vector 2 t a -> Rectangle t a
rectangle = hyperrectangle


hyperrectangleRounded :: Vector dim t a -> Vector (2 ^ dim) t a -> HyperrectangleRounded dim t a
hyperrectangleRounded = HyperrectangleRounded

rectangleRounded :: Vector 2 t a -> Vector 4 t a -> RectangleRounded t a
rectangleRounded = hyperrectangleRounded


halfspace :: Vector dim t a -> Halfspace dim t a
halfspace = Halfspace

halfplane :: Vector 2 t a -> Halfplane t a
halfplane = halfspace
