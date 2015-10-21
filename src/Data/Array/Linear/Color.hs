{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Array.Linear.Color where

import Prelude ()
import Prologue
import Data.Array.Linear


-- === Color modes ===

data RGB
data RGBA
data HSV
data HSVA


-- === Color types ===

data family Color t prec

class IsRGB  c where rgb  :: Iso (Color c prec) (Color c prec') (Color RGB  prec) (Color RGB  prec')
class IsHSV  c where hsv  :: Iso (Color c prec) (Color c prec') (Color HSV  prec) (Color HSV  prec')
class IsRGBA c where rgba :: Iso (Color c prec) (Color c prec') (Color RGBA prec) (Color RGBA prec')
class IsHSVA c where hsva :: Iso (Color c prec) (Color c prec') (Color HSVA prec) (Color HSVA prec')


-- === Mode instances ===
newtype instance Color RGB  prec = RGB  (BVec 3 prec) deriving (Show) 
newtype instance Color HSV  prec = HSV  (BVec 3 prec) deriving (Show) 
newtype instance Color RGBA prec = RGBA (BVec 4 prec) deriving (Show) 
newtype instance Color HSVA prec = HSVA (BVec 4 prec) deriving (Show) 


-- === Conversions ===



