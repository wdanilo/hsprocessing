module Math.Space.Dimension where

import Prologue
import qualified Math.Algebra.Boolean as Bool

data Dim (d :: Nat) t a = Dim (t a) deriving (Show)

type family DimOf (t :: * -> *) :: Nat

type instance DimOf (Dim d t) = d

embed :: Proxy d -> a t -> Dim d a t
embed _ = Dim

embed' :: a t -> Dim d a t
embed' = embed Proxy


type instance DimOf (Bool.Expred t) = DimOf t