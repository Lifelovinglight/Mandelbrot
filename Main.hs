module Main (main) where

import Control.Monad
import Linear
import Data.StateVar
import Foreign.C.Types (CInt)
import qualified SDL
import Control.Monad.Loops
import qualified Graphics.Rendering.OpenGL as OpenGL
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1366, 768)

zoomFactor :: OpenGL.GLfloat
zoomFactor = 0.01

panFactor :: OpenGL.GLfloat
panFactor = 0.01

depthFactor :: OpenGL.GLint
depthFactor = 1

myWindowConfig :: SDL.WindowConfig
myWindowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight
                                   , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL }

keyPressed :: SDL.Event -> Maybe SDL.Keycode
keyPressed event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      if (SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed)
      then Just . SDL.keysymKeycode . SDL.keyboardEventKeysym $ keyboardEvent
      else Nothing
    _ -> Nothing

setUniform :: (OpenGL.Uniform a) => OpenGL.Program -> String -> a -> IO ()
setUniform program name value = get (OpenGL.uniformLocation program name) >>= ($= value) . OpenGL.uniform
  
modUniform :: (OpenGL.Uniform a) => OpenGL.Program -> String -> (a -> a) -> IO ()
modUniform program name fn = get (OpenGL.uniformLocation program name) >>= ($~ fn) . OpenGL.uniform 

data UserAction = PanLeft
                | PanRight
                | PanDown
                | PanUp
                | ZoomIn
                | ZoomOut
                | IncreaseDepth
                | DecreaseDepth
                
keyMapping :: SDL.Keycode -> Maybe UserAction
keyMapping SDL.KeycodeW = Just PanUp
keyMapping SDL.KeycodeS = Just PanDown
keyMapping SDL.KeycodeA = Just PanLeft
keyMapping SDL.KeycodeD = Just PanRight
keyMapping SDL.KeycodeUp = Just ZoomIn
keyMapping SDL.KeycodeDown = Just ZoomOut
keyMapping SDL.KeycodePlus = Just IncreaseDepth
keyMapping SDL.KeycodeMinus = Just DecreaseDepth
keyMapping _ = Nothing

handleKeypress :: OpenGL.Program -> UserAction -> IO ()
handleKeypress program ZoomIn = modUniform program "zoom" (subtract zoomFactor)
handleKeypress program ZoomOut = modUniform program "zoom" (+ zoomFactor)
handleKeypress program PanUp = modUniform program "pany" (+ panFactor)
handleKeypress program PanDown = modUniform program "pany" (subtract panFactor)
handleKeypress program PanRight = modUniform program "panx" (+ panFactor)
handleKeypress program PanLeft = modUniform program "panx" (subtract panFactor)
handleKeypress program IncreaseDepth = modUniform program "depth" (+ depthFactor)
handleKeypress program DecreaseDepth = modUniform program "depth" (subtract depthFactor)

render :: SDL.Window -> IO ()
render window = do
  OpenGL.clear [OpenGL.ColorBuffer]
  OpenGL.currentColor $= OpenGL.Color4 1.0 0.5 0.5 0.5
  OpenGL.renderPrimitive OpenGL.Quads (mapM_ OpenGL.vertex myVertices)
  OpenGL.flush
  SDL.glSwapWindow window
  where myVertices :: [OpenGL.Vertex3 OpenGL.GLfloat]
        myVertices = [ OpenGL.Vertex3 x y 0.0 | (x,y) <- zip [-1.0, 1.0, 1.0, -1.0] [-1.0, -1.0, 1.0, 1.0] ]

loadShaderPair :: String -> String -> IO OpenGL.Program
loadShaderPair fragmentShader vertexShader = do
  frag <- OpenGL.createShader OpenGL.FragmentShader
  fragSource <- readFile fragmentShader
  OpenGL.shaderSourceBS frag $= OpenGL.packUtf8 fragSource
  OpenGL.compileShader frag
  status <- get $ OpenGL.compileStatus frag
  unless status $ putStrLn "Error compiling fragment shader."
  vert <- OpenGL.createShader OpenGL.VertexShader
  vertSource <- readFile vertexShader
  OpenGL.shaderSourceBS vert $= OpenGL.packUtf8 vertSource
  OpenGL.compileShader vert
  status <- get $ OpenGL.compileStatus vert
  unless status $ putStrLn "Error compiling vertex shader."
  program <- OpenGL.createProgram
  OpenGL.attachShader program frag
  OpenGL.attachShader program vert
  OpenGL.linkProgram program
  status <- get $ OpenGL.linkStatus program
  unless status $ putStrLn "Error linking shader program."
  OpenGL.validateProgram program
  status <- get $ OpenGL.validateStatus program
  log <- get $ OpenGL.programInfoLog program
  unless status $ putStrLn log
  return program
                     
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (Text.pack "Mandelbrot") myWindowConfig
  SDL.showWindow window
  SDL.glCreateContext window
  program <- loadShaderPair "Main.frag" "Main.vert"
  OpenGL.currentProgram $= Just program
  
  setUniform program "resolution" (OpenGL.Vector2
                                    ((fromIntegral screenWidth) :: OpenGL.GLfloat)
                                    ((fromIntegral screenHeight) :: OpenGL.GLfloat))
  setUniform program "zoom" (1.0 :: OpenGL.GLfloat)
  setUniform program "panx" (0.0 :: OpenGL.GLfloat)
  setUniform program "pany" (0.0 :: OpenGL.GLfloat)
  setUniform program "depth" (170 :: OpenGL.GLint)

  render window
  iterateWhile not $ do
    event <- SDL.waitEvent
    maybe mempty ((>> render window) . handleKeypress program) (keyPressed event >>= keyMapping) 
    return $ SDL.QuitEvent == SDL.eventPayload event
    
  SDL.destroyWindow window
  SDL.quit
