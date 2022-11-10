module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

maxIter = 1000s

s1 = 200

pointsAsVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [((x/(s1/2)),(y/(s1/2)),0) | x <- [-(s1/2)..(s1/2)], y <- [-(s1/2)..(s1/2)]]


iterCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> GLfloat
iterCount (x0, y0, z0) x y iter | x*x + y*y <= 2*2 && iter < maxIter = iterCount (x0, y0, z0) (x*x - y*y + x0) (2*x*y + y0) (iter + 1)
                         | otherwise = iter

----worst way imaginable to generate two lists----

validPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
validPoints lst = filter (\x -> iterCount x 0 0 0 == maxIter) lst

invalidPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
invalidPoints lst = filter (\x -> iterCount x 0 0 0 /= maxIter) lst

mandlebrotLst = validPoints pointsList
notMandlebrotLst = invalidPoints pointsList

plotColour :: (GLfloat, GLfloat, GLfloat) -> IO ()
plotColour pnt = plot (iterCount pnt 0 0 0)
              where 
              plot :: GLfloat -> IO ()
              plot x = do
                currentColor $= Color4 (x/100) (2*x/100) (3*x/100) 1
                renderPrimitive Points $
                  pointsAsVertexes [pnt]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window "test"
  clearColor $= Color4 1 1 1 1
  mainLoop

window name = do
  createWindow name 
  windowSize $= Size 200 200
  displayCallback $= clear [ColorBuffer]
  displayCallback $= display

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  currentColor $= Color4 0 0 0 1
  
  renderPrimitive Points $
    pointsAsVertexes mandlebrotLst
  repeatNtimes (length notMandlebrotLst) 0
  flush

--not a loop--

repeatNtimes 0 x = return ()
repeatNtimes iter x = do
   plotColour (notMandlebrotLst !! x)
   repeatNtimes (iter-1) (x+1)

----REFERENCES----
{-

https://www.cs.hs-rm.de/~panitz/hopengl/skript.html#tth_sEc1.1
https://wiki.haskell.org/OpenGLTutorial1
https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set

-}



