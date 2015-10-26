{-# LANGUAGE NoMonomorphismRestriction #-}

module Math.Topology.Geometry.Figures where


import Prologue
import Math.Space.Dimension (Dim, embed')

newtype Ball a = Ball a deriving (Show, Functor, Foldable, Traversable)

type Circle a = Dim 2 Ball a


-- TODO: Extend the definition to Regular prism (https://en.wikipedia.org/wiki/Cube)
data Rect a = Rect a a deriving (Show)


ball :: a -> Dim n Ball a
ball = embed' . Ball

circle :: a -> Circle a
circle = ball