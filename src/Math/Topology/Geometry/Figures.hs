{-# LANGUAGE NoMonomorphismRestriction #-}


module Math.Topology.Geometry.Figures where


import Prologue
import Math.Space.Dimension (Dim, DimOf, embed')
import Data.Array.Linear

newtype Ball a = Ball a deriving (Show, Functor, Foldable, Traversable)

type Circle a = Dim 2 Ball a


newtype Hyperrectangle (dim :: Nat) t a = Hyperrectangle (Vector dim t a)

type instance DimOf (Hyperrectangle dim t a) = dim

type Rectangle t a = Hyperrectangle 2 t a

-- data HyperrectangleRounded (dim :: Nat) a
-- = HyperrectangleRounded  (Vec dim a) (Vec (dim * 2) a) deriving (Show, Functor, Foldable, Traversable)


data HyperrectangleRounded b a = HyperrectangleRounded b a deriving (Show, Functor, Foldable, Traversable)

type RectangleRounded b a = Dim 2 (HyperrectangleRounded b) a


newtype Halfspace a = Halfspace a deriving (Show, Functor, Foldable, Traversable)

type Halfplane a = Dim 2 Halfspace a

-- TODO: Extend the definition to Regular prism (https://en.wikipedia.org/wiki/Cube)
-- data Rect a = Rect (Vector 2 a) deriving (Show)


ball :: a -> Dim n Ball a
ball = embed' . Ball

circle :: a -> Circle a
circle = ball


-- hyperrectangleP :: Proxy d -> t -> a -> Hyperrectangle d t a
-- hyperrectangleP = Hyperrectangle

-- hyperrectangle :: t -> a -> Hyperrectangle d t a
-- hyperrectangle = hyperrectangleP Proxy

-- rectangle :: t -> a -> Rectangle t a
-- rectangle = hyperrectangle

hyperrectangle :: Vector dim t a -> Hyperrectangle dim t a
hyperrectangle = Hyperrectangle

rectangle :: Vector 2 t a -> Rectangle t a
rectangle = hyperrectangle


hyperrectangleRounded :: b -> a -> Dim n (HyperrectangleRounded b) a
hyperrectangleRounded = embed' .: HyperrectangleRounded

rectangleRounded :: b -> a -> RectangleRounded b a
rectangleRounded = hyperrectangleRounded


halfspace :: a -> Dim n Halfspace a
halfspace = embed' . Halfspace

halfplane :: a -> Halfplane a
halfplane = halfspace
