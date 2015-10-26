{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.Rendering.WebGL where

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

compileShader = liftIO . jsInitShaders . pack
initBuffers = liftIO jsInitBuffers


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

getElementById = js1 "getElementById"
getCanvas = do
    document <- jsg "document"
    document ^. getElementById "canvas"



---------------------------------------------------


data Mesh a = Mesh { _vertices :: [a]
                   , _itemSize :: Int
                   } deriving (Show)

makeLenses ''Mesh

triangular = flip Mesh 3



rectGeo w h = triangular [   w,  h,  0
                         ,  -w,  h,  0
                         ,   w, -h,  0
                         ,  -w, -h,  0
                         ] :: Mesh Float



---------------------------------------------------



myInitGL :: MakeObjectRef s => s -> JSM JSValueRef
myInitGL canvas = do
    ctx <- canvas ^. getContext "webgl"
    ctx ^. viewportWidth  <# canvas ^. width
    ctx ^. viewportHeight <# canvas ^. height
    return ctx


initCanvas = do 
    canvas <- getCanvas
    ctx    <- myInitGL canvas
    liftIO $ jsRegisterGL (castRef ctx) -- REMOVE ME
    ctx ^. clearColor (0.0 :: Float) (0.3 :: Float) (0.0 :: Float) (1.0 :: Float)
    ctx ^. enable (ctx ^. js_DEPTH_TEST)
    liftIO js_test
    return ctx


clearScene ctx = do
    m1  <- makeValueRef =<< ctx ^. js_COLOR_BUFFER_BIT
    m2  <- makeValueRef =<< ctx ^. js_DEPTH_BUFFER_BIT
    mod <- liftIO $ js_or (castRef m1) (castRef m2) 
    ctx ^. js_clear mod



drawObject ctx (buff, cbuff, coordBuff, uvBuff) program gw gh = do
    liftIO prepareScene

    mat4  <- jsg "mat4"
    posMx <- jsg "mvMatrix"



    mat4  ^. js_translate posMx posMx (eval $ show $ ([350.0, 350.0, -7.0] :: [Float]))

    ctx   ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) buff
    ctx   ^. js_vertexAttribPointer (program ^. js_vertexPositionAttribute)
                                    (buff    ^. js_itemSize)
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

    ctx ^. js_drawArrays (ctx ^. js_TRIANGLE_STRIP) (0 :: Int) (buff ^. js_numItems)


--setUni1f =  

makeRectGeo ctx gw gh = do
    let g2 = rectGeo gw gh

    buff <- ctx ^. createBuffer
    ctx ^. js_bindBuffer (ctx ^. js_ARRAY_BUFFER) buff
    ctx ^. bufferData (ctx ^. js_ARRAY_BUFFER) (new "Float32Array" $ eval $ show $ g2 ^. vertices) (ctx ^. js_STATIC_DRAW)

    buff ^. js_itemSize <# pToJSRef (g2 ^. itemSize)
    buff ^. js_numItems <# pToJSRef (length (g2 ^. vertices) `quot` g2 ^. itemSize)
            

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

    return (buff, cbuff, coordBuff, uvBuff)
