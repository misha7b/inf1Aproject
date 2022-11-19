module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Fixed


maxIter = 100000
zoom = (0.005)

startReal = -zoom -- -2
endReal = zoom --  0.47

startIm = -zoom -- -1.12
endIm = zoom     -- 1.12

shiftReal = -0.745  --(-0.7463)
shiftIm =  0.1 --(0.1102)

width = 800
height = 800

wSize = Size 800 800
pallateSize = 30

genPallate :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
genPallate n = [((x*360/n),1,1) | x<- [1..n+1]]

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


smoothColouring n z = n + 1 - log(logBase 2 z)
        

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

{-
pointsList :: [(GLfloat, GLfloat, GLfloat)]
pointsList = [ ((startReal + (x/width) * (endReal-startReal)),(startIm + (y/height) * (endIm-startIm)),0)    | x <- [0..width], y <- [0..height] ]
-}

scaledPoints :: [(GLfloat, GLfloat, GLfloat)]
scaledPoints = [ (x,y,0)   | x <- [0..width], y <- [0..height] ]

toComplex (x,y,z) = ((startReal + (x/width) * (endReal-startReal)),(startIm + (y/height) * (endIm-startIm)),0)


mandelbrotCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> (GLfloat,GLfloat)
mandelbrotCount (x0, y0, z0) x y iter | x*x + y*y <= 4 && iter < maxIter = mandelbrotCount (x0, y0, z0) (x*x - y*y + x0+ shiftReal) (2*x*y + y0 + shiftIm) (iter + 1)
                                      | otherwise = (iter,sqrt(x*x + y*y))



juliaCount :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> GLfloat -> (GLfloat, GLfloat)
juliaCount (x0, y0, z0) x y iter | x0 * x0 + y0 * y0 <= 4 && iter < maxIter = juliaCount ((x0 * x0 - y0 * y0 + x ), (2*x0*y0 + y), 0) x y (iter + 1)
                         | otherwise = (iter,sqrt(x0*x0 + y0*y0))




-- chooses the function to plot and the starting values

fn = mandelbrotCount

c1 = 0
c2 = 0

--c1 = 0.28
--c2 = 0.008

--c1 = -0.162
--c2 = 1.04

--c1 = -0.79
--c2 = 0.15

--douady rabbit
--c1 = -0.1226
--c2 = -0.7449

--toComplex 

--colours points in the that go to infinity black and other 

plotColour :: (GLfloat, GLfloat, GLfloat) -> IO ()
plotColour pnt = plot (fn (toComplex(pnt)) c1 c2 0)
              where 
              plot :: (GLfloat,GLfloat) -> IO ()
              plot (n,z) = do
                --print(n)
                let action |n == maxIter = do
                                currentColor $= Color4 0 0 0 1
                                renderPrimitive Points $
                                  vertex3f pnt
                           
                           |otherwise = do
                                
                                let smoothVal = (smoothColouring n z)
                               
                                
                                --1. smooth colouring
                                let val = hsvToRGB ((smoothVal*360/1000), 1, 1)
                                 
                                --2. HSV colouring
                                --let val = hsvToRGB ((((n*360/75)**1.5)`mod'` 360),1,1)

                                --3. pallete coloruing best for large zooms
                                --let val =(hsvToRGB(hsvPallate !! floor(n `mod'`pallateSize)))

                                currentColor $= Color4  ((chooseColour val !! 0 )) (chooseColour val !! 1) (chooseColour val !! 2) 1
                                renderPrimitive Points $
                                  vertex3f (scalePnt pnt) 
                                
                                where 
                                  chooseColour :: (GLfloat, GLfloat, GLfloat) -> [GLfloat]
                                  chooseColour (x,y,z) = [x,y,z]
                                  scalePnt (x,y,z) = (((x-(width/2))/(width/2)), ((y-(height/2))/(height/2)), z)
                                  
                              
                      
                                  
                action            
               
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window "test"
  --print ( pointsList == (pointsTest scaledPoints))
  clearColor $= Color4 0 0 0 1
  projection (0) 0 (0) 0 (0) 0
  mainLoop

projection xl xu yl yu zl zu = do 
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0

window name = do
  createWindow name 
  windowSize $= wSize
  displayCallback $= clear [ColorBuffer]
  displayCallback $= display

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  currentColor $= Color4 0 0 0 1
  repeatNtimes (length scaledPoints) 0
  flush

--not a loop--

repeatNtimes 0 x = return ()
repeatNtimes iter x = do
   plotColour (scaledPoints !! x)
   repeatNtimes (iter-1) (x+1)

----REFERENCES----
{-

https://www.cs.hs-rm.de/~panitz/hopengl/skript.html#tth_sEc1.1
https://wiki.haskell.org/OpenGLTutorial1
https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set
https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB
http://www.cuug.ab.ca/dewara/mandelbrot/Mandelbrowser.html
-}



