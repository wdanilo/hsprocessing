module Language.GLSL.JSBuilder where

import           Control.Monad.State                (State)
import           GHCJS.Types                        (JSRef)
import           Graphics.Rendering.WebGL           (compileShader)
import           Prelude                            ()
import           Prologue                           hiding (Bounded, div, void, (.=), (.>))
import           Language.GLSL.Builder

type JSProgram        = Program JSRef

compileMaterial :: (MonadIO m, GLSLBuilder t (State GLSLState) u) => t -> m (JSProgram u)
compileMaterial obj = do
    (glsl, u) <- compileGLSL obj
    flip Program u <$> compileShader glsl
