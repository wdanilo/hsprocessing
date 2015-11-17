{-# LANGUAGE NoMonomorphismRestriction #-}

module Math.Topology.Geometry.Figures where


import Prologue
import Math.Space.Dimension (Dim, embed')

newtype Ball a = Ball a deriving (Show, Functor, Foldable, Traversable)

type Circle a = Dim 2 Ball a


newtype Hyperrectangle a = Hyperrectangle a deriving (Show, Functor, Foldable, Traversable)

type Rectangle a = Dim 2 Hyperrectangle a


newtype Halfspace a = Halfspace a deriving (Show, Functor, Foldable, Traversable)

type Halfplane a = Dim 2 Halfspace a

-- TODO: Extend the definition to Regular prism (https://en.wikipedia.org/wiki/Cube)
-- data Rect a = Rect (Vector 2 a) deriving (Show)


ball :: a -> Dim n Ball a
ball = embed' . Ball

circle :: a -> Circle a
circle = ball

hyperrectangle :: a -> Dim n Hyperrectangle a
hyperrectangle = embed' . Hyperrectangle

rectangle :: a -> Rectangle a
rectangle = hyperrectangle

