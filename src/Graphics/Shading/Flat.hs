module Graphics.Shading.Flat where

import Prologue

import Graphics.Shading.Pattern


-- === Flat shading layers ===

type Exp     = Float

data Layer a = Fill         (Pattern a)
             | Border a     (Pattern a)
             | Shadow a Exp (Pattern a)
             deriving (Show, Functor, Traversable, Foldable)