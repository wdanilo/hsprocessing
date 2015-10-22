{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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

type family Replicate (num :: Nat) a :: [*] where Replicate 0 a = '[]
                                                  Replicate n a = (a ': Replicate (n-1) a)




data R (dim :: Nat) = R deriving (Show)

newtype Space base t a = Space t deriving (Show)

type family SpaceBase space where SpaceBase (Space base t a) = base
type family SpaceType space where SpaceType (Space base t a) = t
type family SpaceUnit space where SpaceUnit (Space base t a) = a

type family SpaceOf (t :: * -> *) :: * -> *

type instance SpaceOf (Space base t) = Space base t

--type family CoordsOf a 
type family CoordsOf (t :: * -> *) :: * -> *


type family DimOf a :: Nat

type instance DimOf (RTuple ()) = 0
type instance DimOf (RTuple (a,as)) = 1 + DimOf (RTuple as)

type instance CoordsOf (Cartesian dim) = A.BVec dim 

type SpaceCoordsOf t = CoordsOf (SpaceOf t)

-- === Space types 

data Space_Type_Cartesian = Space_Type_Cartesian deriving (Show)

type Cartesian dim = Space (R dim) Space_Type_Cartesian




--class HasPosition t where
--    position :: Lens' (t a) (SpaceCoordsOf t a)



--data Position = Position deriving (Show)
--type instance Attrib Position = Expr

type family Attrib a (t :: * -> *) :: *

--type family SpaceOf t :: *

type family UnitOfM (m :: * -> *) :: *

--class Provides d m where
--    get :: d -> Attrib d m


--ball :: _ => _
ball r p = ("sdf_ball" [convert p, convert r] :: Expr)


--v :: _ => _
v = fromList [1, 2, 3] :: A.BVec 3 Float

--linear :: (VecData dim a ~ RTuple t, dim ~ DimOf (RTuple t)) => RTuple t -> Linear dim a 
--linear = Linear

v' = convert v :: Expr

instance (Convertible a Expr, KnownNat dim) => Convertible (A.BVec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ toList v


vec0_ :: A.BVec 0 t
vec1_ :: t -> A.BVec 1 t
vec2_ :: t -> t -> A.BVec 2 t
vec3_ :: t -> t -> t -> A.BVec 3 t

vec0_          = fromList []
vec1_ t1       = fromList [t1]
vec2_ t1 t2    = fromList [t1,t2]
vec3_ t1 t2 t3 = fromList [t1,t2,t3]



----------------------------------------------------

data family Tx (dim :: Nat) a


--data RGB   = RGB Float Float Float deriving (Show)


data Material = Solid (Color RGB Float) deriving (Show)
           -- | Gradient

--data Colored       a = Colored     RGB   a deriving (Show, Functor)
data Bounded     s a = Bounded     s     a deriving (Show, Functor)
data XFormed     s a = XFormed     [s]   a deriving (Show, Functor) -- [s] => mat4 / mat3 s


--data XForm = XForm deriving (Show)
data Style = Style { _fill_s :: Maybe Material } deriving (Show)

makeLenses ''Style

instance Default Style where
    def = Style def

--data XForm = XForm
--type XForm = Quaternion Float


--class HasXForm a where xform :: Lens' a XForm

--translate :: A.BVec 3 a 
--translate x y z a = Repa.mmultS 







newtype Geom g (dim :: Nat) a = Geom (Tx dim a) 

--deriving instance Show (V dim a) => Show (Geom dim a)

newtype Sphere a = Sphere a deriving (Show, Functor)
newtype Ball   a = Ball   a deriving (Show, Functor)


newtype SDF space a = SDF { runSDF :: SpaceCoordsOf space a -> a }

class IsSDF space shape a where
    sdf :: shape a -> SDF space a

instance IsSDF (Cartesian 2) Ball Expr where
    sdf (Ball r) = SDF $ \p -> ("sdf_ball" [convert p, r] :: Expr)

--class IsSDF a where
--    sdf :: t a -> a

ball_sdf :: Convertible a Expr => a -> SDF (Cartesian 2) Expr
ball_sdf r = SDF $ \p -> ("sdf_ball" [convert p, convert r] :: Expr)


data Shape space a = Shape (A.BQuaternion a) Style (SDF space a) 
                   | Merge (Shape space a) (Shape space a)
                   | Diff  (Shape space a) (Shape space a)

--s1 = Style (Just $ Solid $ fromList )
b1 :: Shape (Cartesian 2) Expr
b1 = Shape mempty def (sdf $ Ball 100.0)


magicPosV2 :: A.BVec 2 Expr
magicPosV2 = vec2_ ("p" .> "x") ("p" .> "y")




--buildGLSL :: Shape (Cartesian 2) Expr -> Expr
--buildGLSL (Shape xform style sdf) = runSDF sdf magicPosV2


data StdUniforms = StdUniforms { _position :: A.BVec 2 Expr 
                               , _color    :: Expr
                               } deriving (Show)
makeLenses ''StdUniforms

data GLSLState = GLSLState { _stdUniforms :: StdUniforms
                           , _names       :: [String] 
                           } deriving (Show)
makeLenses ''GLSLState

instance Monoid StdUniforms where
    mempty = StdUniforms { _position = vec2_ ("p" .> "x") ("p" .> "y")
                         , _color    = "gl_FragColor"
                         }

instance Monoid GLSLState where
    mempty = GLSLState mempty ((\s -> ("_" <> s <> "_")) .: flip (:) <$> fmap show [0..] <*> ['a' .. 'z'])

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
getColor    = view color    <$> getStdUniforms 

---------------------

class GLSLBuilder t m where
    buildGLSL :: t -> m [Statement]


instance MonadGLSL m => GLSLBuilder (Shape (Cartesian 2) Expr) m where
    buildGLSL (Shape xform style sdf) = do
        
        p      <- getPosition
        g      <- newName "sdf"
        fill   <- newName "fill"
        border <- newName "border"
        shadow <- newName "shadow"
        color  <- getColor

        let fillStat = case style ^. fill_s of
                Nothing -> []
                Just i  -> [ val vec4 fill $ "vec4" [1.0,0.4,0.0, "sdf_aa"[g]]
                           , color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", fill .> "a"], 1.0]
                           ]

        return $ [ val float g $ runSDF sdf p
                 -- , val vec4  fill    $ "vec4" [1.0,0.4,0.0, "sdf_aa"[g]]
                 , val vec4  border  $ "vec4" [1.0,1.0,1.0, "sdf_aa"["sdf_borderOut" [10.0, g]]]
                 , val vec4  shadow  $ "sdf_shadow" [g-10.0, 20.0, 0.2, 2.0]
                 , color .= "vec4" [0.1,0.1,0.1,1.0]
                 , color .= "vec4" ["mix" [color .> "rgb", shadow .> "rgb", shadow .> "a"], 1.0] 
                 , color .= "vec4" ["mix" [color .> "rgb","gradient_hsv" ["cartesian2polar"["translate"["p","vec2"[100.0,100.0]]],10.0], border .> "a"], 1.0] 
                 -- , color .= "vec4" ["mix" [color .> "rgb", fill .> "rgb", fill .> "a"], 1.0] 
                 ] <> fillStat


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


    let c = RGB $ fromList [1,2,3] :: Color RGB Float
        d = c & (wrapped' . A.x) .~ 10 
        e = c & wrapped .~ (fromList [1,2,3]) :: Color RGB Int


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
--type        SpaceCoordsBase t = CoordsOf (SpaceOf t)
--type        SpaceCoords t = SpaceCoordsBase t (SpaceUnits t)


--type family Units a
--type        SpaceUnits t = Units (SpaceOf t)

--class HasPosition t where
--    position :: Lens' (t u) (SpaceCoordsBase t u)

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