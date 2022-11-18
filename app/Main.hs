module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Fixed


maxIter = 1000

s = 300
wSize = Size 300 300

pallateSize = 50

genPallate :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
genPallate n = [((x*360/n),1,1) | x<- [0..n]]

hsvPallate = genPallate pallateSize

--converts HSV to RGB--

hsvToRGB :: (GLfloat,GLfloat,GLfloat) -> (GLfloat, GLfloat, GLfloat)
hsvToRGB (h,s,v) | 0 <= hPrime && hPrime < 1 = calcRGB (c,x,0)
                 | 1 <= hPrime && hPrime < 2 = calcRGB (x,c,0)
                 | 2 <= hPrime && hPrime < 3 = calcRGB (0,c,x)
                 | 3 <= hPrime && hPrime < 4 = calcRGB (0,x,c)
                 | 4 <= hPrime && hPrime < 5 = calcRGB (x,0,c)
                 | otherwise         = calcRGB (c,0,x)
              where 
                c = v*s
                hPrime = h / 60
                x = c*(1 - abs((hPrime `mod'` 2)-1))
                m = v-c
                calcRGB (r1, g1, b1) = ((r1 + m), (g1 + m), (b1 + m))





smoothColouring n z = 1 + n + logBase 2 (logBase 2 (z))/ log(2)

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [((x/(s/2)),(y/(s/2)),0) | x <- [-(s/2)..(s/2)], y <- [-(s/2)..(s/2)]]


mandlebrotCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> (GLfloat,GLfloat)
mandlebrotCount (x0, y0, z0) x y iter | x*x + y*y <= 4 && iter < maxIter = mandlebrotCount (x0, y0, z0) (x*x - y*y + x0 -0.5) (2*x*y + y0) (iter + 1)
                         | otherwise = (iter,(sqrt(x*x + y*y)))


juliaCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> (GLfloat, GLfloat)
juliaCount (x0, y0, z0) x y iter | x0 * x0 + y0 * y0 <= 2*2 && iter < maxIter = juliaCount ((x0 * x0 - y0 * y0 + x ), (2*x0*y0 + y), 0) x y (iter + 1)
                         | otherwise = (iter,x0)

-- chooses the function to plot and the starting values

fn = juliaCount
--c1 = 0
--c2 = 0
c1 = -0.79
c2 = 0.15


--colours points in the that go to infinity black and other 

plotColour :: (GLfloat, GLfloat, GLfloat) -> IO ()
plotColour pnt = plot (fn pnt c1 c2 0)
              where 
              plot :: (GLfloat,GLfloat) -> IO ()
              plot (iter,y) = do
                let action |iter == maxIter = do
                                currentColor $= Color4 0 0 0 1
                                renderPrimitive Points $
                                  vertex3f pnt
                            |otherwise = do
                                
                                
                                let val =(hsvToRGB(hsvPallate !! floor(iter `mod'` 50)))
                                
                                currentColor $= Color4  (chooseColour val !! 0) (chooseColour val !! 1) (chooseColour val !! 2) 1
                                renderPrimitive Points $
                                  vertex3f pnt 
                                
                                where 
                                  chooseColour :: (GLfloat, GLfloat, GLfloat) -> [GLfloat]
                                  chooseColour (x,y,z) = [x,y,z]

                                  
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
https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB
-}



