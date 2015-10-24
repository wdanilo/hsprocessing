{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GLSL.Builder where

import           Prelude                        ()
import           Prologue                       hiding (div, (.>), (.=), void)
import           Language.GLSL                  ()
import           Language.GLSL.Syntax
import           Text.PrettyPrint.HughesPJClass (prettyShow, Pretty)
import           Data.String                    (IsString, fromString)
import qualified Data.Char as Char
import           Data.Convert
import           GHC.TypeLits                   (Nat)
import qualified Control.Monad.State           as State
import           GHC.TypeLits
import           Control.Monad.State           (State, runState, evalState, execState, get, put)
import           Data.Array.Linear.Color.Class
import           Data.Array.Linear.Color.Modes
import           Data.Array.Linear.Color.Attrs.RGBA

--import qualified Data.Array.Repa as Repa
--import           Data.Array.Repa hiding (Shape)
--import qualified Data.Array.Repa.Matrix as Repa
--import qualified Data.Array.Repa.Repr.Unboxed as Repa

--import           Data.Vector.Unboxed (Unbox)
--import           Data.Array.Repa.Matrix (Matrix, Quaternion)
--import qualified Data.Array.Repa.Matrix as Mx

import qualified Data.Vector as V

--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Array.Repa.Repr.Vector as Repa

import qualified Data.Array.Linear as A

import Data.RTuple

import Language.GLSL.DSL

import Math.Algebra.Boolean hiding (Expr)
import qualified Math.Algebra.Boolean as Bool
import Math.Space.Metric.SDF
import Math.Topology.Geometry.Figures
import Math.Space.Dimension (Dim(..), DimOf)



instance (Convertible a Expr, KnownNat dim) => Convertible (A.BVec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v












-- === StdUniforms ===


data StdUniforms = StdUniforms { _position :: A.BVec 2 Expr 
                               , _colorx   :: Expr
                               } deriving (Show)
makeLenses ''StdUniforms



instance Monoid StdUniforms where
    mempty = StdUniforms { _position = A.vec2 ("p" .> "x") ("p" .> "y")
                         , _colorx   = "gl_FragColor"
                         }

-- === GLSLState ===

data GLSLState = GLSLState { _stdUniforms :: StdUniforms
                           , _names       :: [String] 
                           } deriving (Show)
makeLenses ''GLSLState


-- Instances

class Monad m => MonadGLSL m where
    getState :: m GLSLState
    putState :: GLSLState -> m ()

instance MonadGLSL (State GLSLState) where
    getState = get
    putState = put


instance Monoid GLSLState where
    mempty = GLSLState mempty ((\s -> ("_" <> s <> "_")) .: flip (:) <$> ("" : fmap show [0..]) <*> ['a' .. 'z'])

-- Utils

genName' :: MonadGLSL m => m String
genName' = do
    s <- getState
    let (n:ns) = view names s
    putState (s & names .~ ns)
    return n

genName :: MonadGLSL m => m Expr
genName = fromString <$> genName'

newName' :: MonadGLSL m => String -> m String
newName' pfx = (pfx <>) <$> genName'

newName :: MonadGLSL m => String -> m Expr
newName pfx = fromString <$> newName' pfx

getStdUniforms = view stdUniforms <$> getState

getPosition = view position <$> getStdUniforms 
getColor    = view colorx   <$> getStdUniforms 





-- === Pattern ===

data Pattern a = Solid (Color RGBA a) deriving (Show, Functor, Traversable, Foldable)
          -- | Gradient

-- === Material ===

data Material a = Material [Layer a] deriving (Show, Functor, Traversable, Foldable)

instance Default (Material a) where
    def = Material mempty

class HasMaterial t where
    material :: Lens' (t a) (Material a)

-- === Material.Layer.Flat ===

type F_Radius = Float
type F_Exp    = Float

data Layer a = Fill           (Pattern a)
             | Border a       (Pattern a)
             | Shadow a F_Exp (Pattern a)
             deriving (Show, Functor, Traversable, Foldable)




-- === Shape ===



--data Object t a = Object (A.BQuaternion a) (Material a) (Bool.Expr t) deriving (Show)
--type Shape a = Object (SDF Expr) a
--object = Object mempty def . return


data Transformed t a = Transformed (A.BQuaternion a) (t a) deriving (Show, Functor, Traversable, Foldable)
data Shaded      t a = Shaded      (Material a)      (t a) deriving (Show, Functor, Traversable, Foldable)

newtype Object     t a = Object (Shaded (Transformed t) a) deriving (Show, Functor, Traversable, Foldable)
--newtype BoolObject t a = BoolObject (Object t (Bool.Expr a)) deriving (Show, Functor, Traversable, Foldable)
type BoolObject t = Object (Bool.Expred t)

--data Object t a = Object (A.BQuaternion a) (Material a) (Bool.Expr t) deriving (Show)

type Shape a = Object (SDF 2)


type instance DimOf (Object t) = DimOf t

--object :: (Num a, Monad t) => t a -> BoolObject t a
--object = Object . Shaded def . Transformed mempty . lift

object :: SDF 2 Expr -> BoolObject (SDF 2) Expr
object = Object . Shaded def . Transformed mempty . Bool.Expred . Bool.Val


instance HasMaterial (Shaded t) where
   material = lens (\(Shaded m _) -> m) (\(Shaded _ t) m -> Shaded m t)

instance HasMaterial (Object t) where
   material = wrapped . material



instance Rewrapped (Object t a) (Object t' a')
instance Wrapped   (Object t a) where
    type Unwrapped (Object t a) = Shaded (Transformed t) a
    _Wrapped' = iso (\(Object a) -> a) Object



type instance DimOf (Shaded t) = DimOf t
type instance DimOf (Transformed t) = DimOf t


---------------------


instance Convertible a Expr => Convertible (Dim 2 Ball a) (SDF 2 Expr) where
    convert (Dim (Ball r)) = SDF $ \v -> "sdf_ball" [convert v, convert r]


---------------------





---------------------

class GLSLBuilder t m where
    buildGLSL :: t -> m [Statement]


--instance Convertible (Color RGBA Float) Expr where
--    convert c = "vec4" [ cxonvert $ c ^. x, convert $ c ^. y, convert $ c ^. z, convert $ c ^. w ]  

instance Convertible a Expr => Convertible (Color RGBA a) Expr where
    convert (view wrapped -> c) = "vec4" [ convert $ c ^. A.x, convert $ c ^. A.y, convert $ c ^. A.z, convert $ c ^. A.w ]  



instance (Convertible (t a) (SDF 2 Expr), MonadGLSL m, Convertible a Expr)
      => GLSLBuilder (BoolObject t a) m where
    buildGLSL (Object (Shaded (Material layers) (Transformed xform (Bool.Expred (Bool.Val obj))))) = do

        let sdf = convert obj :: SDF 2 Expr
        
        p      <- getPosition
        
        
        color  <- getColor
        gstart <- newName "sdf"


        let drawPattern = \case 
                Solid c -> do
                    fill <- newName "fill"
                    return (fill, [ val vec4 fill $ convert c ])

            processLayer = \case
                Fill pattern -> do
                    let gtrans g = return (g,[])
                        gdraw g = do
                            (fill, glsl) <- drawPattern pattern
                            return $ glsl 
                                  <> [ color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", "sdf_aa"[g] * (fill .> "a") ], 1.0]
                                     ]
                    return (gtrans, gdraw)

                Border rad pattern -> do
                    let gtrans s = do
                            ng     <- newName "sdf"
                            return (ng, [val float ng $ "sdf_grow" [convert rad, s]])
                        gdraw s = do
                            (fill, glsl) <- drawPattern pattern
                            ng     <- newName "sdf"
                            return $ glsl 
                                  <> [ val float ng      $ "sdf_borderOut" [convert rad, s]
                                     , color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", "sdf_aa"[ng] * (fill .> "a") ], 1.0]
                                     ]
                    return (gtrans, gdraw)

                Shadow rad exp pattern -> do
                    shadow <- newName "shadow"
                    let gtrans g = return (g,[])
                        gdraw g = do
                                (fill, glsl) <- drawPattern pattern
                                return $ glsl 
                                      <> [ val float shadow $ "sdf_shadow" [g, convert rad, convert exp]
                                         , color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", shadow], 1.0] 
                                         ]
                    return (gtrans, gdraw)


            drawLayers g []     = return (g,[])
            drawLayers g (l:ls) = do
                (gtrans , gdraw)  <- processLayer l
                (g',  glslGTrans) <- gtrans g
                (g'', glslBgrnd ) <- drawLayers g' ls
                glslLayer         <- gdraw g 
                return (g'', glslGTrans <> glslBgrnd <> glslLayer)

        let rest = [ val float gstart $ runSDF sdf p
                   -- , val vec4  fill    $ "vec4" [1.0,0.4,0.0, "sdf_aa"[g]]
                   , color .= "vec4" [0.1,0.1,0.1,0.0]
                   ]


        (rest <>) . snd <$> drawLayers gstart layers



shapeToGLSL s = prettyShow s1 where

    gExpr = flip evalState (mempty :: GLSLState) $ buildGLSL s

    local  = "local"
    dpr    = "dpr"
    s1 = unit [   func' "main" [ param void ] $ [ val vec3 "local"  $ "world" - "origin" 
                                                , val vec3 "ulocal" $ "local" * "dpr"
                                                , val vec2 "p"      $ ("ulocal" .> "xy")
                                                , val vec2 "c"      $ ("dim" .> "xy") / 2.0
                                                , val vec2 "z"      $ "p" / ("dim" .> "xy")
                                                
                                                ] <> gExpr 
              ]





--instance Unbox Expr

--instance 

--type family CoordsBase space :: * -> *
--type family MetricOf a :: *
--type        Space_CoordsBase t = CoordsOf (SpaceOf t)
--type        Space_Coords t = Space_CoordsBase t (SpaceUnits t)


--type family Units a
--type        SpaceUnits t = Units (SpaceOf t)

--class HasPosition t where
--    position :: Lens' (t u) (Space_CoordsBase t u)

--type family SpaceData d (t :: * -> *)



--type family Provider m (lst :: [*]) :: Constraint where Provider m '[]       = ()
--                                                        Provider m (l ': ls) = (Provides l m, Provider m ls)

--class ProvidesPosition m where
--    getPosition :: m Expr
--    setPosition :: m Expr


----class IsSDF (dim :: Nat) s a where
----    sdf :: s a -> SDF dim 0

--class IsSDFM (dim :: Nat) s m a where
--    sdfM :: s a -> m (SDF dim a)


--instance (Provider m '[Position], Functor m, Convertible a Expr) => IsSDFM 2 Ball m a where
--    sdfM (Ball r) = SDF2 . (\p -> "sdf_circle" [p, convert r]) <$> get Position 


--ball :: Expr -> SDF

--class Merge dim a where merge :: SDF dim a 

--s1 = Shape [circle 12]


--class Foo b s a m where
--    foo :: LensLike ((,) b) s s a b -> (a -> b) -> m b

--class Foo a b c d e f m g where
--    foo :: LensLike a b c d e -> f -> m g



class Foo a b c d where
    foo :: Lens a b c d

--bar :: (Functor f, Foo a b c d) => (c -> f d) -> a -> f b
--bar :: _ => _
--bar = flip State.runState (0 :: Int) $ do
--    x <- view foo
--    y <- view foo
--    c <- set foo y
--    return (x,y)


--bar :: _ => _
--bar a b = a .~ b 