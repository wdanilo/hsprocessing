{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Array.Repa.Matrix where

import Data.Array.Repa                  as R
import Data.Array.Repa.Eval             as R
import Data.Array.Repa.Unsafe           as R
import Data.Array.Repa.Repr.Vector      (V, fromListVector)
import Control.Monad
import Control.Monad.ST.Strict
import Data.Vector.Unboxed (Unbox)
import GHC.TypeLits
import Data.Typeable
import Data.Convert


newtype Matrix (rows :: Nat) (cols :: Nat) a = Matrix (Array V DIM2 a)
type Quaternion = Matrix 4 4

--row :: DIM2 -> Int
--row (Z :. r :. _) = r
--{-# INLINE row #-}


---- | Take the column number of a rank-2 index.
--col :: DIM2 -> Int
--col (Z :. _ :. c) = c
--{-# INLINE col #-}


---- | Transpose a 2D matrix, sequentially.
--transpose2S
--        :: Array V DIM2 a 
--        -> Array V DIM2 a

--transpose2S arr
-- = arr `deepSeqArray`
--   do   computeS
--         $ unsafeBackpermute new_extent swap arr
-- where  swap (Z :. i :. j)      = Z :. j :. i
--        new_extent              = swap (extent arr)
--{-# NOINLINE transpose2S #-}


--mul :: (Num a, Elt a, Unbox a) => Matrix r1 c1 a -> Matrix r2 c2 a -> Matrix r1 c2 a
--mul (Matrix arr) (Matrix brr)
-- = Matrix $ [arr, brr]  `deepSeqArrays` (runST $
--   do   trr     <- R.now $ transpose2S brr
--        let (Z :. h1  :. _)  = extent arr
--        let (Z :. _   :. w2) = extent brr
--        return $ computeS 
--         $ fromFunction (Z :. h1 :. w2)
--         $ \ix   -> R.sumAllS 
--                  $ R.zipWith (*)
--                        (unsafeSlice arr (Any :. (row ix) :. All))
--                        (unsafeSlice trr (Any :. (col ix) :. All)))
--{-# NOINLINE mul #-}


--mul2 :: (Num a) => Matrix r1 c1 a -> Matrix r2 c2 a -> Matrix r1 c2 a
--mul2 (Matrix arr) (Matrix brr)
-- = Matrix $ [arr, brr]  `deepSeqArrays` (runST $
--   do   trr     <- R.now $ transpose2S brr
--        let (Z :. h1  :. _)  = extent arr
--        let (Z :. _   :. w2) = extent brr
--        return $ computeS 
--         $ fromFunction (Z :. h1 :. w2)
--         $ \ix   -> R.sumAllS 
--                  $ R.zipWith (*)
--                        (unsafeSlice arr (Any :. (row ix) :. All))
--                        (unsafeSlice trr (Any :. (col ix) :. All)))
--{-# NOINLINE mul2 #-}


--diagonal :: forall r c a. (Num a, KnownNat r, KnownNat c)
--         => a -> Matrix r c a
--diagonal a = Matrix $ fromListVector (Z :. rows :. cols) $ take (rows * cols) $ cycle pattern where
--    rows    = unsafeConvert $ natVal (Proxy :: Proxy r) :: Int
--    cols    = unsafeConvert $ natVal (Proxy :: Proxy c) :: Int
--    pattern = a : replicate cols 0




instance (Num a, KnownNat r, KnownNat c) => Monoid (Matrix r c a) where
    mempty = undefined


--class Mul a b c | a b -> c where
--  (*) :: a -> b -> c


--quaternion :: Unbox a => [a] -> Array U DIM2 a
--quaternion = fromListUnboxed (Z :. 4 :. 4)

--identity = XForm $ quaternion 
--    [ 1,0,0,0
--    , 0,1,0,0
--    , 0,0,1,0
--    , 0,0,0,1
--    ]

--translation x y z = XForm $ quaternion 
--    [ 1,0,0,0
--    , 0,1,0,0
--    , 0,0,1,0
--    , x,y,z,1
--    ]

--diagonal = 

--instance Num (Matrix rows cols a) where
--  fromInteger i = 
