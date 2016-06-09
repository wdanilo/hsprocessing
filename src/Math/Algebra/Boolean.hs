
module Math.Algebra.Boolean where

import Prologue


data Boolean a = Merge     a a
               | Diff      a a
               | Intersect a a
               deriving (Show, Functor)

data Expr a = Val a
            | Expr (Boolean (Expr a))
            deriving (Show, Functor)

newtype Compound t a = Compound (Expr (t a)) deriving (Show, Functor)

--instance Applicative Expr
--instance Monad Expr where
--	return = Val
	-- >>== : TODO[WD]

--instance MonadTrans Compound where
--	lift = Compound . Val
