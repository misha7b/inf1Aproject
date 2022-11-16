module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT




maxIter = 1000

s = 300
wSize = Size 300 300

c1 = 0
c2 = 0

--pointsAsVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [((x/(s/2)),(y/(s/2)),0) | x <- [-(s/2)..(s/2)], y <- [-(s/2)..(s/2)]]


iterCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> GLfloat
iterCount (x0, y0, z0) x y iter | x*x + y*y <= 2*2 && iter < maxIter = iterCount (x0, y0, z0) (x*x - y*y + x0 -0.5) (2*x*y + y0) (iter + 1)
                         | otherwise = iter

--validPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
--validPoints lst = filter (\x -> iterCount x 0 0 0 == maxIter) lst

--invalidPoints :: [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
--invalidPoints lst = filter (\x -> iterCount x c1 c2 0 /= maxIter) lst

--mandlebrotLst = validPoints pointsList
--notMandlebrotLst = invalidPoints pointsList

--colouring i made up random numbers for it--

plotColour :: (GLfloat, GLfloat, GLfloat) -> IO ()
plotColour pnt = plot (iterCount pnt c1 c2 0)
              where 
              plot :: GLfloat -> IO ()
              plot x = do
                let action |x == maxIter = do
                                currentColor $= Color4 0 0 0 1
                                renderPrimitive Points $
                                  vertex3f pnt
                            |otherwise = do
                                currentColor $= Color4 (2*x/100) (2*x/100) (1*x/100) 1
                                renderPrimitive Points $
                                  vertex3f pnt 
                action            
               
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window "test"
  clearColor $= Color4 0 0 0 1
  mainLoop

window name = do
  createWindow name 
  windowSize $= wSize
  displayCallback $= clear [ColorBuffer]
  displayCallback $= display

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  currentColor $= Color4 0 0 0 1
  repeatNtimes (length pointsList) 0
  flush

--not a loop--

repeatNtimes 0 x = return ()
repeatNtimes iter x = do
   plotColour (pointsList !! x)
   repeatNtimes (iter-1) (x+1)




--generate HSV---

---HSV to RGB---

---orthographic projection---



----REFERENCES----
{-

https://www.cs.hs-rm.de/~panitz/hopengl/skript.html#tth_sEc1.1
https://wiki.haskell.org/OpenGLTutorial1
https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set

-}



