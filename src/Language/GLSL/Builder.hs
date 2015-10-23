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

unit :: [ExternalDeclaration] -> TranslationUnit
unit = TranslationUnit

typeSpec t = TypeSpec Nothing (TypeSpecNoPrecision t Nothing)

void  = typeSpec Void
float = typeSpec Float
vec2  = typeSpec Vec2
vec3  = typeSpec Vec3
vec4  = typeSpec Vec4
mat4  = typeSpec Mat4

param t = ParameterDeclaration Nothing Nothing t Nothing

var = Variable

sub = Sub
mul = Mul
add = Add
div = Div

app name params = FunctionCall (FuncId name) (Params params)


func tp name args body = FunctionDefinition (FuncProt (FullType Nothing tp) name args) (Compound body)
func' = func void

val tp (Variable name) expr = DeclarationStatement (InitDeclaration (TypeDeclarator (FullType Nothing tp))  [ InitDecl name Nothing (Just expr) ])


a .> fields = FieldSelection a fields

assignment a b = ExpressionStatement (Just (Equal a b))

(.=) = assignment
 

--class Assi
--val 

instance IsString Expr where fromString = var


instance Fractional Expr where
    fromRational = FloatConstant . fromRational
    (/) = div

instance Num Expr where
    fromInteger = IntConstant Decimal . fromInteger
    (*) = mul
    (+) = add
    (-) = sub

instance Convertible Float Expr where
    convert = FloatConstant

--instance IsString TypeSpecifier where
    --fromString (s:ss) = typeSpec $ Char.toUpper s : ss
    --(+), (*), abs, signum, fromInteger, (negate | (-))


instance t ~ Expr => IsString ([t] -> Expr) where fromString = app 





--data Geometry pos  = Geometry pos 

--data Shape = Shape 


--data family V (dim :: Nat) a
--data instance V 1 a = V1 a     deriving (Show)
--data instance V 2 a = V2 a a   deriving (Show)
--data instance V 3 a = V3 a a a deriving (Show)



--xxx :: _ => _
--xxx = V2 1 2


--type Circle a = Geom 2 (Spherical a)

--data Circle a = Circle a

--circle :: Float -> Circle Float
--circle = Circle

--data Rect   a = Rect   a a


--data SDF = forall s. IsSDF s => SDF [s]

--data family SDF (dim :: Nat) a

--data instance SDF 2 a = SDF2 (Expr -> Expr) 
--data instance SDF 2 a = SDF2 Expr deriving (Show)

--data Colored a = Colored Color a
--type SDF = Expr -> Expr

--class Bounded 

--class IsSDF (dim :: Nat) s a where
--    sdf :: (s a) -> SDF dim a

--instance Convertible a Expr => IsSDF 2 Ball a where
--    sdf (Ball r) = SDF2 $ \p -> "sdf_circle" [p, convert r]


---- ===

--type family DimensionsOf a :: Choose Nat

---- ===

--type family Replicate (num :: Nat) a :: [*] where Replicate 0 a = '[]
--                                                  Replicate n a = (a ': Replicate (n-1) a)


--data Choose a = Only a
--              | OneOf [a]
--              | Any
--              deriving (Show)



---- == Coordinate spaces ===

--data Coord_Space coord space dim a = Coord_Space coord (space dim a)

--type Coord_Space_Homogeneous   = Coord_Space Homogeneous
--type Coord_Space_Cartesian     = Coord_Space Cartesian
--type Coord_Space_Spherical     = Coord_Space Spherical
--type Coord_Space_Cylindrical s = Coord_Space Cylindrical s 3


---- === Coordinate systems ===


--data Homogeneous = Homogeneous
--data Cartesian   = Cartesian
--data Spherical   = Spherical
--data Cylindrical = Cylindrical



---- === Metrics ===

--data Discrete  = Discrete
--data Euclidean = Euclidean
--data Taxicab   = Taxicab

--data Metric_Space metric dim a = Metric_Space metric (Space dim a) deriving (Show)

--type Metric_Space_Discrete  = Metric_Space Discrete
--type Metric_Space_Euclidean = Metric_Space Euclidean
--type Metric_Space_Taxicab   = Metric_Space Euclidean


---- === Vector Spaces ===



----- === Spaces ===

--data Space dim a = Space


--type Space_R dim = Space dim Rational
--type Space_I dim = Space dim Integer
--type Space_F dim = Space dim Float



--Metric_Space Euclidean 2 Float



--ball :: _ => _
--ball r p = ("sdf_ball" [convert p, convert r] :: Expr)


--v :: _ => _
v = fromListUnsafe [1, 2, 3] :: A.BVec 3 Float

--linear :: (VecData dim a ~ RTuple t, dim ~ DimOf (RTuple t)) => RTuple t -> Linear dim a 
--linear = Linear

v' = convert v :: Expr

instance (Convertible a Expr, KnownNat dim) => Convertible (A.BVec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v




----------------------------------------------------

data family Tx (dim :: Nat) a


--data RGB   = RGB Float Float Float deriving (Show)




--data Colored       a = Colored     RGB   a deriving (Show, Functor)
data Bounded     s a = Bounded     s     a deriving (Show, Functor)
data XFormed     s a = XFormed     [s]   a deriving (Show, Functor) -- [s] => mat4 / mat3 s


--data XForm = XForm deriving (Show)

--data XForm = XForm
--type XForm = Quaternion Float


--class HasXForm a where xform :: Lens' a XForm

--translate :: A.BVec 3 a 
--translate x y z a = Repa.mmultS 




--
--newtype Ball   a = Ball   a deriving (Show, Functor)


--newtype Geom g (dim :: Nat) a = Geom (Tx dim a) 

----deriving instance Show (V dim a) => Show (Geom dim a)

--newtype Sphere a = Sphere a deriving (Show, Functor)


newtype SDF a = SDF { runSDF :: A.BVec 2 a -> a }

instance Show (SDF a) where show _ = "SDF"

ball :: Convertible r Expr => r -> SDF Expr
ball r = SDF $ \v -> "sdf_ball" [convert v, convert r]









data StdUniforms = StdUniforms { _position :: A.BVec 2 Expr 
                               , _colorx   :: Expr
                               } deriving (Show)
makeLenses ''StdUniforms

data GLSLState = GLSLState { _stdUniforms :: StdUniforms
                           , _names       :: [String] 
                           } deriving (Show)
makeLenses ''GLSLState

instance Monoid StdUniforms where
    mempty = StdUniforms { _position = A.vec2 ("p" .> "x") ("p" .> "y")
                         , _colorx   = "gl_FragColor"
                         }

instance Monoid GLSLState where
    mempty = GLSLState mempty ((\s -> ("_" <> s <> "_")) .: flip (:) <$> ("" : fmap show [0..]) <*> ['a' .. 'z'])

class Monad m => MonadGLSL m where
    getState :: m GLSLState
    putState :: GLSLState -> m ()

instance MonadGLSL (State GLSLState) where
    getState = get
    putState = put

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

data Pattern = Solid (Color RGBA Float) deriving (Show)
          -- | Gradient

-- === Material ===

data Material a = Material [a] deriving (Show)

instance a ~ Flat => Default (Material a) where
    def = Material [ Fill        . Solid $ color4 0.7 0.4 0.0 1.0
                   , Border 10   . Solid $ color4 0.0 1.0 0.0 1.0
                   , Border 4    . Solid $ color4 0.0 0.0 1.0 1.0
                   , Shadow 10 2 . Solid $ color4 0.0 0.0 0.0 0.2
                   ]


-- === Material.Layer.Flat ===

type F_Radius = Float
type F_Exp    = Float

data Flat = Fill                  Pattern
          | Border F_Radius       Pattern
          | Shadow F_Radius F_Exp Pattern
          deriving (Show)




-- === Shape ===

data Shape = Shape (A.BQuaternion Float) (Material Flat) (Boolean (SDF Expr)) deriving (Show)



data Boolean a = B_Value a 
               | B_Merge Float Shape Shape
               | B_Diff        Shape Shape
               deriving (Show)


b1 :: Shape
b1 = Shape mempty def (B_Value $ ball (100.0 :: Float))

---------------------

class GLSLBuilder t m where
    buildGLSL :: t -> m [Statement]


--instance Convertible (Color RGBA Float) Expr where
--    convert c = "vec4" [ cxonvert $ c ^. x, convert $ c ^. y, convert $ c ^. z, convert $ c ^. w ]  

instance Convertible (Color RGBA Float) Expr where
    convert (view wrapped -> c) = "vec4" [ convert $ c ^. A.x, convert $ c ^. A.y, convert $ c ^. A.z, convert $ c ^. A.w ]  



instance MonadGLSL m => GLSLBuilder Shape m where
    buildGLSL (Shape xform (Material layers) (B_Value sdf)) = do
        
        p      <- getPosition
        
        
        color  <- getColor
        gstart <- newName "sdfxx"


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
                                  <> [ color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", "sdf_aa"[g] * (fill .> "a") ], "sdf_aa"[g]]
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


main = do

    let gExpr = flip evalState (mempty :: GLSLState) $ buildGLSL b1

    let local  = "local"
        dpr    = "dpr"
        s1 = unit [   func' "main" [ param void ] $ [ val vec3 "local"  $ "world" - "origin" 
                                                    , val vec3 "ulocal" $ "local" * "dpr"
                                                    --, val mat4 "xform" $ "mat4" $ fmap convert $ Repa.toList mx1
                                                    --, val vec4 "ulocal2" $ ("xform" * ("vec4" [0.0, "ulocal"]))
                                                    --, "ulocal" .= ("ulocal2" .> "xyz")
                                                    -- , val float "g"     $ "sdf_circle" ["translate" ["p", "vec2" [150,160]], 60.0, 60.0]
                                                    , val vec2 "p"      $ ("ulocal" .> "xy") - ("vec2" [150.0,150.0])
                                                    , val vec2 "c"      $ ("dim" .> "xy") / 2.0
                                                    , val vec2 "z"      $ "p" / ("dim" .> "xy")
                                                    
                                                    ] <> gExpr 
                  ]

    let pps = prettyShow s1


    let c = RGB $ fromListUnsafe [1,2,3] :: Color RGB Float
        d = c & (wrapped' . A.x) .~ 10 
        e = c & wrapped .~ (fromListUnsafe [1,2,3]) :: Color RGB Int


    print d
    print e

    --let myv = vec3' 7 2 3 :: Vec' 3 Float
    --let myvE = vec1' "oh" :: Vec' 1 Expr
    --print $ myv ^. x

    return pps

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