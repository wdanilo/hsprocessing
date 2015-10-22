{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


module Data.Array.Linear.Color.Class where

import Prologue
import Data.Array.Linear


-- === Color types ===

data family Color t prec

data RGB
data RGBA
data HSV
data HSVA

newtype instance Color RGB  prec = RGB  (BVec 3 prec) deriving (Show) 
newtype instance Color HSV  prec = HSV  (BVec 3 prec) deriving (Show) 
newtype instance Color RGBA prec = RGBA (BVec 4 prec) deriving (Show) 
newtype instance Color HSVA prec = HSVA (BVec 4 prec) deriving (Show) 


-- === Wrappers ===

instance Wrapped (Color t a) => Rewrapped (Color t a) (Color t a')

instance Wrapped   (Color RGB a) where
    type Unwrapped (Color RGB a) = BVec 3 a
    _Wrapped' = iso (\(RGB v) -> v) RGB

instance Wrapped   (Color HSV a) where
    type Unwrapped (Color HSV a) = BVec 3 a
    _Wrapped' = iso (\(HSV v) -> v) HSV

instance Wrapped   (Color RGBA a) where
    type Unwrapped (Color RGBA a) = BVec 4 a
    _Wrapped' = iso (\(RGBA v) -> v) RGBA

instance Wrapped   (Color HSVA a) where
    type Unwrapped (Color HSVA a) = BVec 4 a
    _Wrapped' = iso (\(HSVA v) -> v) HSVA


-- === Conversions ===

-- TODO: implement

type instance MetaRepr RGB  (Color RGB  a) = Color RGB  a
type instance MetaRepr RGBA (Color RGBA a) = Color RGBA a
type instance MetaRepr HSV  (Color HSV  a) = Color HSV  a
type instance MetaRepr HSVA (Color HSVA a) = Color HSVA a

--instance Meta t (Color t a) where as' = id

--type family MetaRepr t a

--class Meta t a where as' :: t -> Iso' a (MetaRepr t a)