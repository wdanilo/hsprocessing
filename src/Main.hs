{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}


import           Prelude()
import           Prologue            hiding (Bounded)
import           Data.JSString.Text  (lazyTextToJSString)
import           Data.JSString       (unpack, pack, JSString)
import           GHCJS.DOM           (runWebGUI, postGUISync, postGUIAsync, webViewGetDomDocument)
import           GHCJS.DOM.Types     (Element)
import           GHCJS.Types         (JSRef)
import           GHCJS.Marshal       (ToJSRef(..))
import           GHCJS.Marshal.Pure  (PToJSRef(..))
import Language.Javascript.JSaddle
       (strToText, valToStr, JSNull(..), deRefVal, valToObject, js, JSF(..), js0, js1, js2, js3, js4, js5, jsg,
        valToNumber, (!), (!!), (#), (<#), global, eval, fun, val, array, new, runJSaddle_,
        valToText, MakeValueRef(..), JSValue(..), call, JSM(..), JSValueRef, jsf)
import Language.Javascript.JSaddle.Types (castRef)
import Language.Javascript.JSaddle.Classes (MakeObjectRef)

import Control.Lens
import Control.Monad.IO.Class

import qualified Language.GLSL as GLSL
import qualified Language.GLSL.DSL as GLSLDSL
import qualified Text.Parsec   as Parsec

import Text.PrettyPrint.HughesPJClass (prettyShow, Pretty)

import qualified Language.GLSL.Builder as GLSL
import qualified Language.Javascript.JSaddle.String as JS

import Data.Convert
import           Data.Array.Linear.Color.Class
import           Data.Array.Linear.Color.Modes
import qualified Data.Array.Linear as A

import Data.Fixed (mod')

import Control.Monad.State

import Math.Space.Metric.Bounded
import Math.Space.Dimension (Dim)
import Math.Space.Metric.SDF
import Graphics.Shading.Material
import Graphics.Shading.Flat
import Graphics.Shading.Pattern
import qualified Graphics.Display.Object as O
import Graphics.Rendering.GLSL.SDF (object, Object)
import Graphics.Rendering.GLSL.SDF.Figures

import Graphics.Rendering.WebGL




import Control.Concurrent (threadDelay)














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
myRect = Bounded (A.vec2 400 400) (hyperrectangle (A.vec2 120.0 40.0 :: A.BVec 2 GLSL.Expr))
       & material .~ mtl2

myRectR :: Bounded Float (Object 2)
myRectR = Bounded (A.vec2 400 400) (hyperrectangleRounded (A.vec2 120.0 40.0 :: A.BVec 2 GLSL.Expr) (A.vec4 10.0 10.0 10.0 10.0 :: A.BVec 4 GLSL.Expr))
       & material .~ mtl2

-- myHalfPlane :: Bounded Float (Object 2)
myHalfPlane :: Bounded Float (Object 2)
myHalfPlane = Bounded (A.vec2 400 400) (halfspace (A.vec2 1.0 0.0 :: A.BVec 2 GLSL.Expr))
       & material .~ mtl2

-- mySliderLeft :: Bounded Float (Object 2)
-- mySliderLeft = Bounded (A.vec2 400 400) (substract myRect myHalfPlane)

main = do
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

    let objHalfPlane = myHalfPlane
        [gw', gh'] = toList $ objHalfPlane ^. bounds
        gw = gw'/2;
        gh = gh'/2;

    -- let objSliderLeft = mySliderLeft
    --     [gw', gh'] = toList $ objSliderLeft ^. bounds
    --     gw = gw'/2;
    --     gh = gh'/2;


    runWebGUI $ \ webView -> do
        let runjs = postGUIAsync . runJSaddle_ webView

        runjs $ do
            ctx     <- initCanvas

            GLSL.Program jsProg aa <- GLSL.compileMaterial objHalfPlane

            buffers <- makeRectGeo ctx gw gh

            clearScene ctx

            drawObject ctx buffers jsProg gw gh

            --liftIO $ threadDelay 100000

            --clearScene ctx

            --liftIO $ threadDelay 100000

            --clearScene ctx

            --drawObject ctx buffers program gw gh



    -- putStrLn $ ppShow $ Parsec.runParser GLSL.translationUnit GLSL.S "shader parser" shader_t1


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


-- | To parse shader and see its representation use:
-- | `putStrLn $ ppShow $ Parsec.runParser GLSL.translationUnit GLSL.S "shader parser" shader`


type Type = String
type Name = String

data Decl    = Function Name [ArgDecl] deriving (Show)
data ArgDecl = ArgDecl Type Name       deriving (Show)
