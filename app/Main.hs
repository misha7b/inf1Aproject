module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Complex



pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [((x/100),(y/100),0) | x <- [-100..100], y <- [-100..100]]


iterCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> GLfloat
iterCount (x0, y0, z0) x y iter | x*x + y*y <= 2*2 && iter < 1000 = iterCount (x0, y0, z0) (x*x - y*y + x0) (2*x*y + y0) (iter + 1)
                         | otherwise = iter

validPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
validPoints lst = filter (\x -> iterCount x 0 0 0 == 1000) lst


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window "test"
  mainLoop

window name = do
  createWindow name 
  windowSize $= Size 800 800
  displayCallback $= clear [ColorBuffer]
  displayCallback $= display

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (validPoints pointsList)
  flush

--test

{-

references:

https://www.cs.hs-rm.de/~panitz/hopengl/skript.html#tth_sEc1.1
https://wiki.haskell.org/OpenGLTutorial1
https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set


-}
