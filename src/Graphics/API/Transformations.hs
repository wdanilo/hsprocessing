module Graphics.API.Transformations where

import Prologue

import Data.Binary (Binary)

data Translate a = Translate { _dx    :: Int
                             , _dy    :: Int
                             , _trObj :: a
                             } deriving (Show, Eq, Generic)

data Rotate a = Rotate { _deg    :: Int
                       , _rotObj :: a
                       } deriving (Show, Eq, Generic)

data Reflect a = Reflect { _reflObj :: a
                         } deriving (Show, Eq, Generic)

makeLenses ''Translate
makeLenses ''Rotate
makeLenses ''Reflect

instance Binary a => Binary (Translate a)
instance Binary a => Binary (Rotate a)
instance Binary a => Binary (Reflect a)
