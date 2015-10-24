
module Math.Algebra.Boolean where

import Prologue



data Boolean a = Merge Float a a
               | Diff        a a
               deriving (Show, Functor)

data Expr a = Val a
            | Expr  (Boolean (Expr a))
            deriving (Show, Functor)

newtype Expred t a = Expred (Expr (t a)) deriving (Show, Functor)

instance Applicative Expr
instance Monad Expr where
	return = Val
	-- >>== : TODO[WD]

instance MonadTrans Expred where
	lift = Expred . Val