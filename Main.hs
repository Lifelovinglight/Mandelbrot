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

machine :: (Monad m) => (a -> m b) -> (a -> m c) -> a -> m a
machine fa fb value = fa value >> fb value >> return value
fa >>& fb = machine fa fb

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

setUniform :: (OpenGL.Uniform a) => String -> a -> OpenGL.Program -> IO ()
setUniform name value program = get (OpenGL.uniformLocation program name) >>= ($= value) . OpenGL.uniform
  
modUniform :: (OpenGL.Uniform a) => String -> (a -> a) -> OpenGL.Program -> IO ()
modUniform name fn program = get (OpenGL.uniformLocation program name) >>= ($~ fn) . OpenGL.uniform

setCurrentProgram :: OpenGL.Program -> IO ()
setCurrentProgram = (OpenGL.currentProgram $=) . Just

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
handleKeypress program ZoomIn = modUniform "zoom" (subtract zoomFactor) program
handleKeypress program ZoomOut = modUniform "zoom" (+ zoomFactor) program
handleKeypress program PanUp = modUniform "pany" (+ panFactor) program
handleKeypress program PanDown = modUniform "pany" (subtract panFactor) program
handleKeypress program PanRight = modUniform "panx" (+ panFactor) program
handleKeypress program PanLeft = modUniform "panx" (subtract panFactor) program
handleKeypress program IncreaseDepth = modUniform "depth" (+ depthFactor) program
handleKeypress program DecreaseDepth = modUniform "depth" (subtract depthFactor) program

render :: SDL.Window -> IO ()
render window = do
  OpenGL.clear [OpenGL.ColorBuffer]
  OpenGL.currentColor $= OpenGL.Color4 1.0 0.5 0.5 0.5
  OpenGL.renderPrimitive OpenGL.Quads (mapM_ OpenGL.vertex myVertices)
  OpenGL.flush
  SDL.glSwapWindow window
  where myVertices :: [OpenGL.Vertex3 OpenGL.GLfloat]
        myVertices = [ OpenGL.Vertex3 x y 0.0 | (x,y) <- zip [-1.0, 1.0, 1.0, -1.0] [-1.0, -1.0, 1.0, 1.0] ]

loadShaderSource :: String -> OpenGL.Shader -> IO ()
loadShaderSource filePath shader = do
  (OpenGL.shaderSourceBS shader $=) . OpenGL.packUtf8 =<< readFile filePath
  OpenGL.compileShader shader
  checkStatus (OpenGL.compileStatus shader) $ "Error compiling " <> filePath
  
checkStatus :: GettableStateVar Bool -> String -> IO ()
checkStatus getter errormsg = get $ getter >>= flip unless (putStrLn errormsg)

loadShaderPair :: String -> String -> OpenGL.Program -> IO ()
loadShaderPair fragmentShader vertexShader program = do
  OpenGL.createShader OpenGL.FragmentShader
    >>= loadShaderSource fragmentShader
    >>& OpenGL.attachShader program
  OpenGL.createShader OpenGL.VertexShader
    >>= loadShaderSource vertexShader
    >>& OpenGL.attachShader program
  OpenGL.linkProgram program
  checkStatus (OpenGL.linkStatus program) "Error linking shader program." 
  OpenGL.validateProgram program
  checkStatus (OpenGL.validateStatus program) =<< get (OpenGL.programInfoLog program)

initializeUniforms :: OpenGL.Program -> IO OpenGL.Program
initializeUniforms =
  setUniform "resolution" (OpenGL.Vector2
                                    ((fromIntegral screenWidth) :: OpenGL.GLfloat)
                                    ((fromIntegral screenHeight) :: OpenGL.GLfloat))
  >>& setUniform "zoom" (1.0 :: OpenGL.GLfloat)
  >>& setUniform "panx" (0.0 :: OpenGL.GLfloat)
  >>& setUniform "pany" (0.0 :: OpenGL.GLfloat)
  >>& setUniform "depth" (170 :: OpenGL.GLint)

mainLoop :: OpenGL.Program -> SDL.Window -> IO ()
mainLoop program window = void . iterateWhile not $ do
  event <- SDL.waitEvent
  maybe mempty ((>> render window) . handleKeypress program) (keyPressed event >>= keyMapping)
  return $ SDL.QuitEvent == SDL.eventPayload event
    
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (Text.pack "Mandelbrot") myWindowConfig
    >>= SDL.showWindow
    >>& SDL.glCreateContext
  program <- OpenGL.createProgram
    >>= loadShaderPair "Main.frag" "Main.vert"
    >>& setCurrentProgram
    >>& initializeUniforms
  render window
  mainLoop program window
  SDL.destroyWindow window
  SDL.quit
