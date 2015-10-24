module Graphics.Shading.Pattern where

import Prologue
import Data.Array.Linear.Color


data Pattern a = Solid (Color RGBA a) deriving (Show, Functor, Traversable, Foldable)
          -- | Gradient