module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

maxIter = 100

pointsAsVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [((x/1000),(y/1000),0) | x <- [-1000..1000], y <- [-1000..1000]]


iterCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> GLfloat
iterCount (x0, y0, z0) x y iter | x*x + y*y <= 2*2 && iter < maxIter = iterCount (x0, y0, z0) (x*x - y*y + x0) (2*x*y + y0) (iter + 1)
                         | otherwise = iter

validPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
validPoints lst = filter (\x -> iterCount x 0 0 0 == maxIter) lst

mandlebrotLst = validPoints pointsList


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window "test"
  clearColor $= Color4 1 1 1 1
  mainLoop

window name = do
  createWindow name 
  windowSize $= Size 800 800
  displayCallback $= clear [ColorBuffer]
  displayCallback $= display

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  currentColor $= Color4 0 0 0 1
  renderPrimitive Points $
     pointsAsVertexes mandlebrotLst
  flush


{-

-----NOT WORKING-----

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  currentColor $= Color4 0 0 0 1
  renderPrimitive Points $
     pointsAsVertexes mandlebrotLst
  flush
-}






--drawPoints :: IO ()
--drawPoints = mapM_ (\x -> putStrLn $ show i ++ ": " ++ blabla i) validPoints pointsList
--test

--colourChange :: IO ()
--colourChange = mapM (\x ->)

--color3f r g b = color $ Color3 r g (b :: GLfloat)

--vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)


{-




----REFERENCES----

https://www.cs.hs-rm.de/~panitz/hopengl/skript.html#tth_sEc1.1
https://wiki.haskell.org/OpenGLTutorial1
https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set


-}
