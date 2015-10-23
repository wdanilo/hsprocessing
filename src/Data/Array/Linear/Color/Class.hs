{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Array.Linear.Color.Class where

import Prologue


-- === Color type ===

data family         Color t a
type instance Item (Color t a) = a

class IsColor repr t a | repr -> a where
    color :: repr -> Color t a
    

-- === Utilities ==

color1 a       = color (a)
color2 a b     = color (a,b)
color3 a b c   = color (a,b,c)
color4 a b c d = color (a,b,c,d)


-- === Instances ==

instance {-# OVERLAPPABLE #-} (a ~ Item (Unwrapped (Color t a)), Wrapped (Color t a), FromListUnsafe (Unwrapped (Color t a)))
      => FromListUnsafe (Color t a) where
    fromListUnsafe = view unwrapped' . fromListUnsafe