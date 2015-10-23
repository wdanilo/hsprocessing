{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}


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
import qualified Text.Parsec   as Parsec

import Text.PrettyPrint.HughesPJClass (prettyShow, Pretty)

import qualified Language.GLSL.Builder as GLSL
import qualified Language.Javascript.JSaddle.String as JS

--import Language.Haskell.TH
--import Language.Haskell.TH.Quote

--import           GHCJS.DOM.Document  (documentCreateElement, documentGetElementById)
--import Language.Javascript.JSC
--       (eval, evalJM, valToNumber, fun, jsg, js, (#), (<#), runJSC_)

--import Language.Javascript.JSaddle
--        (js2, js1, js0, js, jsg, (!!), (#), (<#), fun,
--         deRefVal, JSValue(..), runJSaddle)

--import GHCJS.DOM.Document (getElementById)

js6 name a0 a1 a2 a3 a4 a5 = jsf name (a0, a1, a2, a3, a4, a5)

foreign import javascript unsafe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript unsafe "alert($1)"
    alert :: JSString -> IO ()


foreign import javascript unsafe "registerGL($1)"
    jsRegisterGL :: JSRef -> IO ()

foreign import javascript unsafe "initShaders($1)"
    jsInitShaders :: JSString -> IO JSRef

foreign import javascript unsafe "initBuffers()"
    jsInitBuffers :: IO ()


foreign import javascript unsafe "test()"
    js_test :: IO ()


foreign import javascript unsafe "prepareScene()"
    prepareScene :: IO ()


foreign import javascript unsafe "($1 | $2)"
    js_or :: JSRef -> JSRef -> IO JSRef

foreign import javascript unsafe "setMatrixUniforms()"
    setMatrixUniforms :: IO ()

foreign import javascript unsafe "console.log($1)"
    js_clog :: JSRef -> IO ()

foreign import javascript unsafe "window.devicePixelRatio"
    js_devicePixelRatio :: IO JSRef

clog = liftIO . js_clog

compileShader = liftIO . jsInitShaders
initBuffers = liftIO jsInitBuffers



--foreign import javascript unsafe "$1['$2']"
--    initBuffers :: IO ()

--"$1.prop = $2;"


foreign import javascript unsafe "$1[$2]"
    getProp' :: JSRef -> JSString -> IO JSRef

getProp s base = getProp' (pToJSRef base) s

getContext     = js1 "getContext"
enable         = js1 "enable"
clearColor     = js4 "clearColor"
width          = js  "width"
height         = js  "height"
viewportWidth  = js  "viewportWidth"
viewportHeight = js  "viewportHeight"
js_DEPTH_TEST  = js  "DEPTH_TEST"

createBuffer        = js0 "createBuffer"
js_bindBuffer       = js2 "bindBuffer"
bufferData          = js3 "bufferData"
js_ARRAY_BUFFER     = js  "ARRAY_BUFFER"
js_STATIC_DRAW      = js  "STATIC_DRAW"
js_COLOR_BUFFER_BIT = js  "COLOR_BUFFER_BIT"
js_DEPTH_BUFFER_BIT = js  "DEPTH_BUFFER_BIT"
js_FLOAT            = js  "FLOAT"
js_TRIANGLE_STRIP   = js  "TRIANGLE_STRIP"
js_TRIANGLES        = js  "TRIANGLES"

js_itemSize     = js  "itemSize"
js_numItems     = js  "numItems"

js_viewport     = js4 "viewport"
js_clear        = js1 "clear"
js_translate    = js3 "translate"
js_create       = js0 "create"
js_drawArrays   = js3 "drawArrays"
js_vertexAttribPointer = js6 "vertexAttribPointer"
js_identity     = js1 "identity"
--js_create       = js6 "vertexAttribPointer"

js_vertexPositionAttribute = js "vertexPositionAttribute"
js_vertexColorAttribute    = js "vertexColorAttribute"
js_vertexCoordAttribute    = js "vertexCoordAttribute"
js_vtxUVAttr               = js "vtxUVAttr"

js_uniform1f               = js2 "uniform1f"
js_uniform2f               = js3 "uniform2f"
js_uniform3f               = js4 "uniform3f"

js_dimUni                  = js "dimUni"
js_dprUni                  = js "dprUni"
js_aaUni                   = js "aaUni"
js_originUni               = js "originUni"

myInitGL :: MakeObjectRef s => s -> JSM JSValueRef
myInitGL canvas = do
    ctx <- canvas ^. getContext "webgl"
    ctx ^. viewportWidth  <# canvas ^. width
    ctx ^. viewportHeight <# canvas ^. height
    return ctx


data Mesh a = Mesh { _vertices :: [a]
                   , _itemSize :: Int
                   } deriving (Show)

makeLenses ''Mesh

triangular = flip Mesh 3



--data Point3 a = Point3 a a a deriving (Show, Functor)
--data Point2 a = Point2 a a   deriving (Show, Functor)

--class           Dim1 p where x :: Lens' (p a) a 
--class Dim1 p => Dim2 p where y :: Lens' (p a) a 
--class Dim2 p => Dim3 p where z :: Lens' (p a) a 

--instance Dim1 Point2 where x = lens (\(Point2 x _ ) -> x) (\(Point2 _ y) x -> Point2 x y)
--instance Dim2 Point2 where y = lens (\(Point2 _ y ) -> y) (\(Point2 x _) y -> Point2 x y)

--instance Dim1 Point2 where x = lens (\(Point2 x _ ) -> x) (\(Point2 _ y) x -> Point2 x y)
--instance Dim2 Point2 where y = lens (\(Point2 _ y ) -> y) (\(Point2 x _) y -> Point2 x y)


rectGeo w h = triangular [   w,  h,  0
                         ,  -w,  h,  0
                         ,   w, -h,  0
                         ,  -w, -h,  0
                         ] :: Mesh Float


--               w     h
data Rect = Rect Float Float deriving (Show)

--class Bounded b where
--    bounds :: b -> Rect

--instance Num a => (Bounded (GLSL.Ball a)) where
--    bounds (GLSL.Ball r) = Rect (2 * convert r) (2 * r)

data Bounded a = Bounded Rect a deriving (Show)

myBall = Bounded (Rect 300 300) GLSL.b1
main = do
    --runJSC_ webView $ do
    --    document <- jsg "document"

    --let getContext = getProp ("getContext" :: JSString)

    let Bounded (Rect gw' gh') geo = myBall
        gw = gw'/2;
        gh = gh'/2;
        smain = GLSL.shapeToGLSL geo

    print "GENERATED:"
    putStrLn smain
    print "-----------------"


    let fullShader = shader_header <> smain


    runWebGUI $ \ webView -> do
        let runjs = postGUIAsync . runJSaddle_ webView

        Just doc <- webViewGetDomDocument webView -- webView.document
    --    --Just body <- documentGetBody doc     -- doc.body
        --Just canvas <- getElementById doc ("canvas" :: JSString)
        let 
            getElementById = js1 "getElementById"
            width          = js  "width"
            height         = js  "height"
            getCanvas = do
                document <- jsg "document"
                document ^. getElementById "canvas"

            getWebGLCtx = do
                canvas <- getCanvas
                canvas ^. getContext "webgl"


        runjs $ do
            canvas <- getCanvas
            ctx    <- myInitGL canvas
            liftIO $ jsRegisterGL (castRef ctx) -- REMOVE ME
            ctx ^. clearColor (0.0 :: Float) (0.3 :: Float) (0.0 :: Float) (1.0 :: Float)
            ctx ^. enable (ctx ^. js_DEPTH_TEST)
            liftIO js_test




            program <- compileShader (pack fullShader)





            let makeRectGeo = do
                    let g2 = rectGeo gw gh

                    buff2 <- ctx ^. createBuffer
                    ctx ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) buff2
                    ctx ^. bufferData (ctx ^. js_ARRAY_BUFFER) (new "Float32Array" $ eval $ show $ g2 ^. vertices) (ctx ^. js_STATIC_DRAW)

                    buff2 ^. js_itemSize <# pToJSRef (g2 ^. itemSize)
                    buff2 ^. js_numItems <# pToJSRef (length (g2 ^. vertices) `quot` g2 ^. itemSize)
                            

                    let colors = [ 1, 0, 0, 1
                                 , 0, 1, 0, 1
                                 , 0, 0, 1, 1
                                 , 0, 0, 0, 1
                                 ] :: [Float]

                    cbuff <- ctx ^. createBuffer
                    ctx ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) cbuff
                    ctx ^. bufferData (ctx ^. js_ARRAY_BUFFER) (new "Float32Array" $ eval $ show $ colors) (ctx ^. js_STATIC_DRAW)
                    cbuff ^. js_itemSize <# (4 :: Int)
                    cbuff ^. js_numItems <# (4 :: Int)


                    -- koordynaty swiata! powinny uwzgledniac przesuniecie obiektu na scenie!
                    let coords = [  gw ,  gh , -7
                                 , -gw ,  gh , -7
                                 ,  gw , -gh , -7
                                 , -gw , -gh , -7
                                 ] :: [Float]

                    coordBuff <- ctx ^. createBuffer
                    ctx ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) coordBuff
                    ctx ^. bufferData (ctx ^. js_ARRAY_BUFFER) (new "Float32Array" $ eval $ show $ coords) (ctx ^. js_STATIC_DRAW)
                    coordBuff ^. js_itemSize <# (3 :: Int)
                    coordBuff ^. js_numItems <# (4 :: Int)


                    let uvs = [ 1, 1
                              , 0, 1
                              , 1, 0
                              , 0, 0
                              ] :: [Float]

                    uvBuff <- ctx ^. createBuffer
                    ctx ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) uvBuff
                    ctx ^. bufferData (ctx ^. js_ARRAY_BUFFER) (new "Float32Array" $ eval $ show $ uvs) (ctx ^. js_STATIC_DRAW)
                    uvBuff ^. js_itemSize <# (2 :: Int)
                    uvBuff ^. js_numItems <# (4 :: Int)

                    return (buff2, cbuff, coordBuff, uvBuff)

            (buff2, cbuff, coordBuff, uvBuff) <- makeRectGeo

            --gl.uniform2f(program.offsetUniform, offset[0], offset[1]);



            m1  <- makeValueRef =<< ctx ^. js_COLOR_BUFFER_BIT
            m2  <- makeValueRef =<< ctx ^. js_DEPTH_BUFFER_BIT
            mod <- liftIO $ js_or (castRef m1) (castRef m2) 
            ctx ^. js_clear mod






            liftIO prepareScene
            --ctx ^. js_viewport (0 :: Int) (0 :: Int) (ctx ^. viewportWidth) (ctx ^. viewportHeight)

            mat4  <- jsg "mat4"
            posMx <- jsg "mvMatrix"

            --mat4  ^. js_translate posMx posMx (eval $ show $ ([-2.5, 0.0, -7.0] :: [Float]))
            --ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) buff
            --ctx   ^. js_vertexAttribPointer (program ^. js_vertexPositionAttribute)
            --                                (buff    ^. js_itemSize)
            --                                (ctx     ^. js_FLOAT)
            --                                False (0 :: Int) (0 :: Int)
            --liftIO setMatrixUniforms
            --ctx ^. js_drawArrays (ctx ^. js_TRIANGLES) (0 :: Int) (buff ^. js_numItems)






            mat4  ^. js_translate posMx posMx (eval $ show $ ([350.0, 350.0, -7.0] :: [Float]))

            ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) buff2
            ctx   ^. js_vertexAttribPointer (program ^. js_vertexPositionAttribute)
                                            (buff2   ^. js_itemSize)
                                            (ctx     ^. js_FLOAT)
                                            False (0 :: Int) (0 :: Int)

            ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) cbuff
            ctx   ^. js_vertexAttribPointer (program ^. js_vertexColorAttribute)
                                            (cbuff   ^. js_itemSize)
                                            (ctx     ^. js_FLOAT)
                                            False (0 :: Int) (0 :: Int)

            ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) coordBuff
            ctx   ^. js_vertexAttribPointer (program   ^. js_vertexCoordAttribute)
                                            (coordBuff ^. js_itemSize)
                                            (ctx       ^. js_FLOAT)
                                            False (0 :: Int) (0 :: Int)


            ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) uvBuff
            ctx   ^. js_vertexAttribPointer (program   ^. js_vtxUVAttr)
                                            (uvBuff    ^. js_itemSize)
                                            (ctx       ^. js_FLOAT)
                                            False (0 :: Int) (0 :: Int)
            

            liftIO setMatrixUniforms

            dpr <- liftIO js_devicePixelRatio
            clog dpr

            ctx ^. js_uniform1f (program ^. js_aaUni)  (0.7 :: Float)
            ctx ^. js_uniform1f (program ^. js_dprUni) dpr
            ctx ^. js_uniform2f (program ^. js_dimUni) gw gh
            ctx ^. js_uniform3f (program ^. js_originUni) (0 :: Float) (0 :: Float) (0 :: Float)

            ctx ^. js_drawArrays (ctx ^. js_TRIANGLE_STRIP) (0 :: Int) (buff2 ^. js_numItems)





        
        --print $ show doc
        print "----------"
        --print $ 
        return ()
    w <- innerWidth
    let str = "trolololo2: " ++ (show w)
    --webGLStart
    --alert $ pack str
    --alert $ pack $ show $ 2**22
    --webGLStart
    --webGLStart
    --webGLStart
    --webGLStart
    --webGLStart
    --let x = prettyShow :: _
    --putStrLn $ prettyShow $ fromRight $ Parsec.runParser GLSL.translationUnit GLSL.S "oh" shader_t1
    putStrLn $ ppShow $ Parsec.runParser GLSL.translationUnit GLSL.S "oh" shader_t1


    



    putStrLn "end"

fromRight = \case
    Right r -> r
    Left  e -> error (show e)

shader_t1 :: String
shader_t1 = [s|void main (void,int) {foo(1,2,3);} |]

shader_header :: String
shader_header = [s|#extension GL_OES_standard_derivatives : enable

    precision mediump float;

    uniform vec2 dim;
    uniform vec3 origin;
    uniform float aa;
    uniform float dpr;

    varying vec4 vColor;
    varying vec3 world;
    varying vec2 uv;


    //////////////////////////////////////
    // Combine distance field functions //
    //////////////////////////////////////

    float length      (float a, float b) { return sqrt(a*a + b*b); }
    float clamp       (float a)          { return clamp (a,0.0,1.0); }
    float invert      (float a)          { return 1.0 - a; }
    float mulInverted (float a, float b) { return invert (invert (a) * invert (b)); }

    float sdf_smoothMerge(float d1, float d2, float k) {
        float dk1 = d1/k;
        float dk2 = d2/k;
        float x = mulInverted(clamp (dk1), clamp (dk2));
        // float x = length(dk1, dk2);
        float h = clamp(0.5 + 0.5*(dk2 - dk1));
        float r = (d1 * h + d2 * invert(h)) - k * h * invert(h);
        r = clamp(r,r/x,r);
        return r;
    }

    float sdf_smoothMergeDisorted(float d1, float d2, float k) {
        float h = clamp(0.5 + 0.5*(d2 - d1)/k, 0.0, 1.0);
        return mix(d2, d1, h) - k * h * (1.0-h);
    }


    float merge     (float d1, float d2) { return min( d1, d2); }
    float sdf_subtract  (float d1, float d2) { return max(-d1, d2); }
    float sdf_intersect (float d1, float d2) { return max( d1, d2); }
    float sdf_grow      (float size, float d) { return d - size; }
    float sdf_shrink    (float size, float d) { return sdf_grow(-size,d); }


    //////////////////////////////
    // Distance field functions //
    //////////////////////////////


    float sdf_pie(vec2 p, float angle) {
        angle = radians(angle) / 2.0;
        vec2 n = vec2(cos(angle), sin(angle));
        return abs(p).x * n.x + p.y*n.y;
    }


    float sdf_ball(vec2 p, float radius) {
        return length(p) - radius;
    }

    float sdf_ball(vec2 p, float radius, float angle) {
        return sdf_subtract(sdf_pie(p, angle), sdf_ball(p,radius));
    }

    float sdf_sphere(vec2 p, float radius) {
        return abs(sdf_ball(p,radius));
    }

    float sdf_ellipse(vec2 p, float a, float b) {
        float a2  = a * a;
        float b2  = b * b;
        float px2 = p.x * p.x;
        float py2 = p.y * p.y;
        return (b2 * px2 + a2 * py2 - a2 * b2)/(a2 * b2);
    }


    float sdf_triangle(vec2 p, float radius) {
        return max( abs(p).x * 0.866025 + 
                    p.y * 0.5, -p.y) 
                    -radius * 0.5;
    }


    float sdf_triangle(vec2 p, float width, float height) {
        vec2 n = normalize(vec2(height, width / 2.0));
        return max( abs(p).x*n.x + p.y*n.y - (height*n.y), -p.y);
    }

    float sdf_ring(vec2 p, float radius, float width) {
        width /= 2.0;
        radius -= width;
        return abs(sdf_ball(p, radius)) - width;
    }

    float sdf_ring(vec2 p, float radius, float width, float angle) {
       return sdf_subtract(sdf_pie(p, angle), sdf_ring(p, radius, width));
    }

    float sdf_rect(vec2 p, vec2 size) {
        size /= 2.0;
        vec2 d = abs(p) - size;
        return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    }

    float sdf_rect(vec2 p, vec2 size, float radius) {
        size /= 2.0;
        size -= vec2(radius);
        vec2 d = abs(p) - size;
        return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - radius;
    }

    float sdf_rect(vec2 p, vec2 size, vec4 corners) {
        float tl = corners[0];
        float tr = corners[1];
        float bl = corners[2];
        float br = corners[3];

        size /= 2.0;

             if (p.x <  - size.x + tl && p.y >   size.y - tl ) { return length (p - vec2(- size.x + tl,   size.y - tl)) - tl; }
        else if (p.x >    size.x - tr && p.y >   size.y - tr ) { return length (p - vec2(  size.x - tr,   size.y - tr)) - tr; }
        else if (p.x <  - size.x + bl && p.y < - size.y + bl ) { return length (p - vec2(- size.x + bl, - size.y + bl)) - bl; }
        else if (p.x >    size.x - br && p.y < - size.y + br ) { return length (p - vec2(  size.x - br, - size.y + br)) - br; }
        else {
            vec2 d = abs(p) - size;
            return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
        }
    }


    float sdf_line(vec2 p, vec2 start, vec2 end, float width) {
        vec2 dir = start - end;
        float lngth = length(dir);
        dir /= lngth;
        vec2 proj = max(0.0, min(lngth, dot((start - p), dir))) * dir;
        return length( (start - p) - proj ) - (width / 2.0);
    }


    ///////////////////////
    // Masks for drawing //
    ///////////////////////


    float sdf_fill(float dist) {
        return clamp(-dist, 0.0, 1.0);
    }


    // float sdf_borderOut(float width, float p) {
    //     float alpha1 = clamp(p);
    //     float alpha2 = clamp(p - width);
    //     return sdf_subtract (sdf_shrink(width,p),p);
    // }

    float sdf_borderOut(float width, float p) {
        return sdf_subtract (p + 0.5,sdf_grow(width,p));
    }

    float sdf_borderIn(float width, float p) {
        float alpha1 = clamp(p);
        float alpha2 = clamp(p - width);
        return sdf_subtract (p,sdf_grow(width,p));
    }

    float sdf_shadow(float p, float width, float exp) {
        return pow(1.0-clamp(p/width),exp);
    }

    


    /////////////////////
    // Transformations //
    /////////////////////


    vec2 sdf_rotateCCW(vec2 p, float a) {
        mat2 m = mat2(cos(a), sin(a), -sin(a), cos(a));
        return p * m;   
    }


    vec2 sdf_rotateCW(vec2 p, float a) {
        mat2 m = mat2(cos(a), -sin(a), sin(a), cos(a));
        return p * m;
    }


    vec2 translate(vec2 p, vec2 t) {
        return p - t;
    }


    // ---------------------------

    vec3 hsv2rgb(vec3 c) {
        vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
        vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
        return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
    }



    float sdf_aa(float d) {
        float anti = fwidth(d) * aa;
        return 1.0-smoothstep(-anti, anti, d);
    }

    vec2 cartesian2polar (vec2 p) {
        return vec2(length(p), atan(p.y, p.x));
    }

    vec3 gradient_hsv (vec2 p, float step) {
        return hsv2rgb(vec3(p.x/step,1.0,1.0)); 
    }


    vec2 appTrans2D (vec2 p, mat4 xform) {
        return (xform * vec4(p,0.0,1.0)).xy;
    }

    float bismooth (float a, float exp) {
        if (a > 0.5) { return 1.0 - pow((1.0 - a) * 2.0, exp)/2.0; } 
        else         { return pow(a * 2.0, exp)/2.0;               }
    }

    vec3 smoothMerge (float d1, float d2, vec3 c1, vec3 c2, float width) {
        return mix (c1,c2,bismooth(clamp((d1-d2+2.0*width)/(4.0*width)),2.0));
    }
|]

type Type = String
type Name = String

data Decl    = Function Name [ArgDecl] deriving (Show)
data ArgDecl = ArgDecl Type Name       deriving (Show)