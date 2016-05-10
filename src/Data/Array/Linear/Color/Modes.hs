{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}


module Data.Array.Linear.Color.Modes where

import Prologue -- hiding (FromListUnsafe, fromListUnsafe, Item, ToList, FromList, toList, fromList)
import Data.Array.Linear
import Data.Array.Linear.Color.Class
import Constraint.Container.Homo     (Homo)
import Data.Repr.Meta                (MetaRepr, Meta, as')

-- import Data.List.Class


-- === Color types ===

data RGB
data RGBA
data HSV
data HSVA

newtype instance Color RGB  a = RGB  (BVec 3 a) deriving (Show, Functor, Foldable, Traversable)
newtype instance Color HSV  a = HSV  (BVec 3 a) deriving (Show, Functor, Foldable, Traversable)
newtype instance Color RGBA a = RGBA (BVec 4 a) deriving (Show, Functor, Foldable, Traversable)
newtype instance Color HSVA a = HSVA (BVec 4 a) deriving (Show, Functor, Foldable, Traversable)


-- === Constructors ===

instance Homo (t,t2,t3)    => IsColor (t,t2,t3)    RGB  t where color (r,g,b)   = fromListUnsafe [r,g,b]
instance Homo (t,t2,t3)    => IsColor (t,t2,t3)    HSV  t where color (h,s,v)   = fromListUnsafe [h,s,v]

instance Homo (t,t2,t3,t4) => IsColor (t,t2,t3,t4) RGBA t where color (r,g,b,a) = fromListUnsafe [r,g,b,a]
instance Homo (t,t2,t3,t4) => IsColor (t,t2,t3,t4) HSVA t where color (h,s,v,a) = fromListUnsafe [h,s,v,a]


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

type instance MetaRepr RGB  (Color RGB  a) = Color RGB  a
type instance MetaRepr RGBA (Color RGBA a) = Color RGBA a
type instance MetaRepr HSV  (Color HSV  a) = Color HSV  a
type instance MetaRepr HSVA (Color HSVA a) = Color HSVA a

instance {-# OVERLAPPABLE #-} MetaRepr t (Color t a) ~ Color t a
      => Meta t (Color t a) where as' _ = id

-- TODO: implement non-trivial conversions
