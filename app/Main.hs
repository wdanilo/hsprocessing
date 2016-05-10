{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}


import           Control.Concurrent                  (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.Array.Linear                   as A
import           Data.Array.Linear.Color.Class
import           Data.Array.Linear.Color.Modes
import           Data.Convert
import           Data.Fixed                          (mod')
import           Data.JSString                       (JSString, pack, unpack)
import           Data.JSString.Text                  (lazyTextToJSString)
import           GHCJS.DOM                           (postGUIAsync, postGUISync,
                                                      runWebGUI,
                                                      webViewGetDomDocument)
import           GHCJS.DOM.Types                     (Element)
import           GHCJS.Marshal                       (ToJSRef (..))
import           GHCJS.Marshal.Pure                  (PToJSRef (..))
import           GHCJS.Types                         (JSRef)
import qualified Graphics.Display.Object             as O
import           Graphics.Rendering.GLSL.SDF         (Object, diff, merge,
                                                      object, translate)
import           Graphics.Rendering.GLSL.SDF.Figures
import           Graphics.Rendering.WebGL
import           Graphics.Shading.Flat
import           Graphics.Shading.Material
import           Graphics.Shading.Pattern
import qualified Language.GLSL                       as GLSL
import qualified Language.GLSL.Builder               as GLSL
import qualified Language.GLSL.JSBuilder             as GLSL
import qualified Language.GLSL.DSL                   as GLSLDSL
import           Language.Javascript.JSaddle         (JSF (..), JSM (..),
                                                      JSNull (..), JSValue (..),
                                                      JSValueRef,
                                                      MakeValueRef (..), array,
                                                      call, deRefVal, eval, fun,
                                                      global, js, js0, js1, js2,
                                                      js3, js4, js5, jsf, jsg,
                                                      new, runJSaddle_,
                                                      strToText, val,
                                                      valToNumber, valToObject,
                                                      valToStr, valToText, (!),
                                                      (!!), ( # ), (<#))
import           Language.Javascript.JSaddle.Classes (MakeObjectRef)
import qualified Language.Javascript.JSaddle.String  as JS
import           Language.Javascript.JSaddle.Types   (castRef)
import           Math.Space.Dimension                (Dim)
import           Math.Space.Metric.Bounded
import           Math.Space.Metric.SDF
import           Prelude                             ()
import           Prologue                            hiding (Bounded)
import qualified Text.Parsec                         as Parsec
import           Text.PrettyPrint.HughesPJClass      (Pretty, prettyShow)





---------------------
-- === Example === --
---------------------

data BSize = BSize deriving (Show)
type instance GLSL.UniformType BSize = Float
instance GLSL.IsUniformID BSize where reprID _ = "bsize"

data BRad = BRad deriving (Show)
type instance GLSL.UniformType BRad = Float
instance GLSL.IsUniformID BRad where reprID _ = "brad"


--myBall2 = do
--    bsize <- GLSL.newUniform2 BSize 10.0
--    brad  <- GLSL.newUniform2 BRad  10.0


mtl      = Material $ [ Fill            . Solid $ color4 0.7 0.2 0.2 1.0
                      , Border 10.0     . Solid $ color4 0.0 1.0 0.0 1.0
                      , Shadow 10.0 2.0 . Solid $ color4 0.0 0.0 0.0 0.2
                      ] :: Material (Layer GLSL.Expr)

mtl2     = Material $ [ Fill            . Solid $ color4 0.6 0.6 0.6 1.0
                      ] :: Material (Layer GLSL.Expr)

mtl3     = Material $ [ Fill            . Solid $ color4 0.3 0.3 0.3 1.0
                      ] :: Material (Layer GLSL.Expr)



--Material [AA, BSize] Expr

--mtl2 = do
--    bsize <- GLSL.newUniform2 BSize (10.0 :: Float)
--    let mtl = Material $ [ Fill                   . Solid $ color4 0.7 0.2 0.2 1.0
--                         , Border (convert bsize) . Solid $ color4 0.0 1.0 0.0 1.0
--                         , Shadow 10.0 2.0        . Solid $ color4 0.0 0.0 0.0 0.2
--                         ] :: Material (Layer GLSL.Expr)
--    return mtl

myBall :: Bounded Float (Object 2)
myBall = Bounded (A.vec2 400 400) (ball 100.0)
       & material .~ mtl


-- test :: _ -> _
-- test a = view A.x a


myRect :: Bounded Float (Object 2)
myRect = Bounded (A.vec2 400 400) (hyperrectangle (A.vec2 180.0 40.0 :: A.BVec 2 GLSL.Expr))
       & material .~ mtl2

-- myRectR :: Bounded Float (Object 2)
-- myRectR = Bounded (A.vec2 400 400) (translate (A.vec3 180.0 10.0 0.0)
--     (hyperrectangleRounded (A.vec2 120.0 40.0 :: A.BVec 2 GLSL.Expr) (A.vec4 10.0 10.0 10.0 10.0 :: A.BVec 4 GLSL.Expr)))
--        & material .~ mtl2

myRectL :: Bounded Float (Object 2)
myRectL = Bounded (A.vec2 400 400) (hyperrectangleRounded (A.vec2 180.0 20.0 :: A.BVec 2 GLSL.Expr) (A.vec4 10.0 10.0 10.0 10.0 :: A.BVec 4 GLSL.Expr))
       & material .~ mtl2

myRectR :: Bounded Float (Object 2)
myRectR = Bounded (A.vec2 400 400) (hyperrectangleRounded (A.vec2 180.0 20.0 :: A.BVec 2 GLSL.Expr) (A.vec4 10.0 10.0 10.0 10.0 :: A.BVec 4 GLSL.Expr))
       & material .~ mtl3

value = 10.0

-- myHalfPlane :: Bounded Float (Object 2)
myHalfPlaneL :: Bounded Float (Object 2)
myHalfPlaneL = Bounded (A.vec2 400 400) (translate (A.vec3 value 0.0 0.0) (halfspace (A.vec2 1.0 0.0 :: A.BVec 2 GLSL.Expr)))
       & material .~ mtl3


myHalfPlaneR :: Bounded Float (Object 2)
myHalfPlaneR = Bounded (A.vec2 400 400) (translate (A.vec3 value 0.0 0.0) (halfspace (A.vec2 (-1.0) 0.0 :: A.BVec 2 GLSL.Expr)))
       & material .~ mtl2


myMix :: Bounded Float (Object 2)
myMix = Bounded (A.vec2 400 400) (merge (myRect ^. bounded) (myHalfPlaneL ^. bounded))

mySliderL :: Bounded Float (Object 2)
mySliderL = Bounded (A.vec2 400 400) (diff (myHalfPlaneR ^. bounded) (myRectL ^. bounded)) & material .~ mtl2

mySliderR :: Bounded Float (Object 2)
mySliderR = Bounded (A.vec2 400 400) (diff (myHalfPlaneL ^. bounded) (myRectR ^. bounded)) & material .~ mtl3

mySlider :: Bounded Float (Object 2)
mySlider = Bounded (A.vec2 400 400) (merge (mySliderL ^. bounded) (mySliderR ^. bounded)) & material .~ mtl


main = do
    putStrLn "HSProcessing test started."

    let objBall = myBall
        [gw', gh'] = toList $ objBall ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let objRect = myRect
        [gw', gh'] = toList $ objRect ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let objRectR = myRectR
        [gw', gh'] = toList $ objRectR ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let objMix = myMix
        [gw', gh'] = toList $ objMix ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    let objSlider = mySlider
        [gw', gh'] = toList $ objSlider ^. bounds
        gw = gw'/2;
        gh = gh'/2;


    runWebGUI $ \ webView -> do
        let runjs = postGUIAsync . runJSaddle_ webView

        runjs $ do
            ctx     <- initCanvas

            GLSL.Program jsProg aa <- GLSL.compileMaterial objBall

            buffers <- makeRectGeo ctx gw gh

            clearScene ctx

            drawObject ctx buffers jsProg gw gh

            --liftIO $ threadDelay 100000

            --clearScene ctx

            --liftIO $ threadDelay 100000

            --clearScene ctx

            --drawObject ctx buffers program gw gh



    putStrLn $ ppShow $ Parsec.runParser GLSL.translationUnit GLSL.S "shader parser" shader_t5

    putStrLn "HSProcessing test finished."


--zrobic datatype Program ktory bedzie wrapperem na RTuple uniformow, kotra bedziemy mogli adresowac lensami
--kazdy uniform ma osobny typ wtedy




fromRight = \case
    Right r -> r
    Left  e -> error (show e)






shader_t1 :: String
shader_t1 = [s|uniform float aa = 1.0; |]

shader_t2 :: String
shader_t2 = [s|vec2 v = vec2(2.0, 3.0); |]

shader_t3 :: String
shader_t3 = [s|void main(void) { vec2 a = vec2(2.0, 3.0); } |]

shader_t4 :: String
shader_t4 = [s|void main(void) { float a = 1.0; fun1(); fun2(); x = fun3(); } |]

shader_t5 :: String
shader_t5 = [s|float a = 1.0; fun1(); fun2(); x = fun3();|]


-- | To parse shader and see its representation use:
-- | `putStrLn $ ppShow $ Parsec.runParser GLSL.translationUnit GLSL.S "shader parser" shader`


type Type = String
type Name = String

data Decl    = Function Name [ArgDecl] deriving (Show)
data ArgDecl = ArgDecl Type Name       deriving (Show)
