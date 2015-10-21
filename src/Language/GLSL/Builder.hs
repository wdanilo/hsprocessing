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

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa hiding (Shape)
import qualified Data.Array.Repa.Matrix as Repa
import qualified Data.Array.Repa.Repr.Unboxed as Repa

import           Data.Vector.Unboxed (Unbox)
import           Data.Array.Repa.Matrix (Matrix, Quaternion)
import qualified Data.Array.Repa.Matrix as Mx

import qualified Data.Vector as V

import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Repa.Repr.Vector as Repa

import qualified Data.Vector.Matrix as MM

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

val tp name expr = DeclarationStatement (InitDeclaration (TypeDeclarator (FullType Nothing tp))  [ InitDecl name Nothing (Just expr) ])


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


--data Layout_RTuple = Layout_RTuple deriving (Show)

--type family VLayout style (dim :: Nat) a
--type instance VLayout Layout_RTuple dim a = VecData dim a

--newtype Linear (dim :: Nat) a = Linear (VLayout Layout_RTuple dim a)


--type VecData dim a = AsRTuple (Replicate dim a)
type VecData dim a = AsTList a (Replicate dim a)


--newtype Linear (dim :: Nat) a = Linear (VecData dim a)

--deriving instance Show (VecData dim a) => Show (Linear dim a)

data Vec (dim :: Nat) a = Vec {fromVec :: V.Vector a} deriving (Show)

data Vec' (dim :: Nat) a = Vec' (Array Repa.V DIM1 a)

--data instance Vec 0 a = V0 deriving (Show)
--data instance Vec 1 a = V1 !a deriving (Show)
--data instance Vec 2 a = V2 !a !a deriving (Show)
--data instance Vec 3 a = V3 !a !a !a deriving (Show)


--newtype Vec (dim :: Nat) a = Vec (VecData dim a)

instance Wrapped (Vec dim a) where
    type Unwrapped (Vec dim a) = V.Vector a
    _Wrapped' = iso (\(Vec a) -> a) Vec


instance Wrapped (Vec' dim a) where
    type Unwrapped (Vec' dim a) = Array Repa.V DIM1 a
    _Wrapped' = iso (\(Vec' a) -> a) Vec'


--type Expr_V (dim :: Nat) = V dim Expr


--newtype Vec dim a = Vec (V dim (Linear dim a))

--deriving instance Show (VecData dim a) => Show (Vec dim a)


--type family NatElem (n :: Nat) a
--type instance NatElem 0 (RTuple (r,rs)) = r
--type instance NatElem n (RTuple (r,rs)) = NatElem (n-1) (RTuple rs)


--class NatIdx (n :: Nat) 


unsafeAt :: Int -> Lens' (V.Vector a) a
unsafeAt i = lens (flip V.unsafeIndex i) undefined

unsafeRepaAt :: Int -> Lens' (Array Repa.V DIM1 a) a
unsafeRepaAt i = lens (\v -> v Repa.! (Z :. i)) (\v a -> (\v' -> Repa.fromVector (Z:.(V.length v')) v') $ V.unsafeUpd (Repa.toVector v) [(i,a)])
--unsafeRepaAt i = lens (\v -> v Repa.! (Z :. i)) (\v a -> Repa.fromUnboxed $ VU.unsafeUpd (Repa.toUnboxed v) [(i,a)])

class Dim1 (t :: Nat -> * -> *) d where x   :: Lens' (t d a) a
class Dim1 t d => Dim2 t        d where y   :: Lens' (t d a) a
                                        yx  :: Lens' (t d a) (t 2 a)
                                        xy  :: Lens' (t d a) (t 2 a)
class Dim2 t d => Dim3 t        d where z   :: Lens' (t d a) a
                                        zx  :: Lens' (t d a) (t 2 a)
                                        xz  :: Lens' (t d a) (t 2 a)
                                        zy  :: Lens' (t d a) (t 2 a)
                                        yz  :: Lens' (t d a) (t 2 a)
                                        xyz :: Lens' (t d a) (t 3 a)




class CheckDim (min :: Nat) (dim :: Nat) 
instance CheckDim min dim -- FIXME: TODO

instance CheckDim 1 dim => Dim1 Vec dim where x = wrapped . unsafeAt 0
instance CheckDim 2 dim => Dim2 Vec dim where y = wrapped . unsafeAt 1
instance CheckDim 3 dim => Dim3 Vec dim where z = wrapped . unsafeAt 2

instance CheckDim 1 dim => Dim1 Vec' dim where x = wrapped . unsafeRepaAt 0
instance CheckDim 2 dim => Dim2 Vec' dim where y = wrapped . unsafeRepaAt 1
instance CheckDim 3 dim => Dim3 Vec' dim where z = wrapped . unsafeRepaAt 2


--instance CheckDim 1 dim => Dim1 Vec' dim where x = wrapped . unsafeRepaAt 0


--instance 

type family ElemsType a

class                                      Elems t el where elems :: t -> [el]

instance                                   Elems (RTuple ())      el where elems _             = []
instance (t ~ el, Elems (RTuple ts) el) => Elems (RTuple (t,ts))  el where elems (RTuple (t,ts)) = t : elems (RTuple ts)

instance                                          Elems (TList x ())      el where elems _              = []
instance (t ~ el, x ~ t, Elems (TList x ts) el)  => Elems (TList x (t,ts))  el where elems (TList (t,ts)) = t : elems (TList ts :: TList x ts)



--instance (el ~ a, Elems (VecData dim a) a) => Elems (Linear dim a) el where elems (Linear a) = elems a

--instance Elems a el                         => Elems (V dim a)      el where elems (V a)      = elems a
--instance (el ~ a, Elems (VecData dim a) a) => Elems (Vec dim a) el where elems (Vec a) = elems a 

--type family ElementOf a

--type instance ElementOf (RTuple )

--type family Attrib a


--type family DataOf Position t = DataOf PositionOf t (UnitOf t)

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

type instance CoordsOf (Cartesian dim) = Vec dim 

type SpaceCoordsOf t = CoordsOf (SpaceOf t)

-- === Space types 

data Space_Type_Cartesian = Space_Type_Cartesian deriving (Show)

type Cartesian dim = Space (R dim) Space_Type_Cartesian




class HasPosition t where
    position :: Lens' (t a) (SpaceCoordsOf t a)



--data Position = Position deriving (Show)
--type instance Attrib Position = Expr

type family Attrib a (t :: * -> *) :: *

--type family SpaceOf t :: *

type family UnitOfM (m :: * -> *) :: *

class Provides d m where
    get :: d -> Attrib d m


--ball :: _ => _
ball r p = ("sdf_ball" [convert p, convert r] :: Expr)


--v :: _ => _
v = vec3_ 1 2 3 :: Vec 3 Float

--linear :: (VecData dim a ~ RTuple t, dim ~ DimOf (RTuple t)) => RTuple t -> Linear dim a 
--linear = Linear

v' = convert v :: Expr

instance (Convertible a Expr, KnownNat dim) => Convertible (Vec dim a) Expr where
    convert v = fromString ("vec" <> show (natVal (Proxy :: Proxy dim))) $ fmap convert $ V.toList $ fromVec v



vec0_ = Vec $ V.fromList []
vec1_ t1 = Vec $ V.fromList [t1]
vec2_ t1 t2 = Vec $ V.fromList [t1,t2]
vec3_ t1 t2 t3 = Vec $ V.fromList [t1,t2,t3]

vec0' :: Vec' 0 a
vec0'       = Vec' $ Repa.fromListVector (Z :. 0) []

vec1' :: a -> Vec' 1 a
vec1' x     = Vec' $ Repa.fromListVector (Z :. 1) [x]

vec2' :: a -> a -> Vec' 2 a
vec2' x y   = Vec' $ Repa.fromListVector (Z :. 2) [x,y]

vec3' :: a -> a -> a -> Vec' 3 a
vec3' x y z = Vec' $ Repa.fromListVector (Z :. 3) [x,y,z]

----------------------------------------------------

data family Tx (dim :: Nat) a


data RGB   = RGB Float Float Float deriving (Show)

data Color = Color deriving (Show)

data Colored       a = Colored     RGB   a deriving (Show, Functor)
data Bounded     s a = Bounded     s     a deriving (Show, Functor)
data XFormed     s a = XFormed     [s]   a deriving (Show, Functor) -- [s] => mat4 / mat3 s


--data XForm = XForm deriving (Show)
data Style = Style deriving (Show)

type XForm = Quaternion Float

quaternion :: Unbox a => [a] -> Array U DIM2 a
quaternion = fromListUnboxed (Z :. 4 :. 4)

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

class HasXForm a where xform :: Lens' a XForm

--translate :: 
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


data Shape space a = Shape XForm Style (SDF space a) 

b1 = Shape mempty Style (sdf $ Ball 100.0)


magicPosV2 :: Vec 2 Expr
magicPosV2 = vec2_ ("p" .> "x") ("p" .> "y")

buildGLSL :: Shape (Cartesian 2) Expr -> Expr
buildGLSL (Shape _ _ sdf) = runSDF sdf magicPosV2


repaTest = fromListUnboxed (Z :. 3 :. 5) [1..15] :: Array U DIM2 Int




main = do

    let gExpr = buildGLSL b1

    let local  = "local"
        dpr    = "dpr"
        s1 = unit [   func' "main" [ param void ] [ val vec3 "local"  $ "world" - "origin" 
                                                  , val vec3 "ulocal" $ "local" * "dpr"

                                                  --, val mat4 "xform" $ "mat4" $ fmap convert $ Repa.toList mx1
                                                  --, val vec4 "ulocal2" $ ("xform" * ("vec4" [0.0, "ulocal"]))
                                                  --, "ulocal" .= ("ulocal2" .> "xyz")

                                                  , val vec2 "p"      $ "ulocal" .> "xy"
                                                  , val vec2 "c"      $ ("dim" .> "xy") / 2.0
                                                  , val vec2 "z"      $ "p" / ("dim" .> "xy")

                                                  , val float "g" gExpr
                                                  -- , val float "g"     $ "sdf_circle" ["translate" ["p", "vec2" [150,160]], 60.0, 60.0]
                                                  , val vec4  "l1"    $ "vec4" [1.0,0.4,0.0, "sdf_aa"["g"]]
                                                  , val vec4  "l2"    $ "vec4" [1.0,1.0,1.0, "sdf_aa"["g" - 10.0]]
                                                  , val vec4  "l3"    $ "sdf_shadow" ["g"-10.0, 20.0, 0.2, 2.0]

                                                  , "gl_FragColor" .= "vec4" [0.1,0.1,0.1,1.0]
                                                  , "gl_FragColor" .= "vec4" ["mix" ["gl_FragColor" .> "rgb", "l3" .> "rgb", "l3" .> "a"], 1.0] 
                                                  , "gl_FragColor" .= "vec4" ["mix" ["gl_FragColor" .> "rgb","gradient_hsv" ["cartesian2polar"["translate"["p","vec2"[100.0,100.0]]],10.0], "l2" .> "a"], 1.0] 
                                                  , "gl_FragColor" .= "vec4" ["mix" ["gl_FragColor" .> "rgb", "l1" .> "rgb", "l1" .> "a"], 1.0] 
                                                  ]
                  ]

    let pps = prettyShow s1
    print repaTest
    print $ repaTest ! (Z:.2:.1)


    let myv = vec3' 7 2 3 :: Vec' 3 Float
    let myvE = vec1' "oh" :: Vec' 1 Expr
    print $ myv ^. x

    MM.main
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