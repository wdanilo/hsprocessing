module Math.Space.Dimension where

import Prologue
import Math.Algebra.Boolean (Compound)



-- === Dimensions ===

data Dim (d :: Nat) t a = Dim (t a) deriving (Show)

type family   DimOf t :: Nat
type instance DimOf (Dim d t a) = d

-- utils

embed :: Proxy d -> a t -> Dim d a t
embed _ = Dim

embed' :: a t -> Dim d a t
embed' = embed Proxy

convertInDim :: (Convertible a b, DimOf a ~ DimOf b) => a -> b
convertInDim = convert


-- === External instances ===

type instance DimOf (Compound t a) = DimOf (t a)