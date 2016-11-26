{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Graphics.UI.GLUT
import Data.IORef


{----------------------------------  Main and Display ----------------------------}

-- Main Function
-- Yeah I have no idea what any of this does
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  iter <- newIORef 1
  displayCallback $= display iter
  reshapeCallback $= Just reshape
  --idleCallback $= Just (idle iter)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: IORef Int -> DisplayCallback
display iter = do
  --clear [ColorBuffer]
  --a <- get iter
  draw (gradientImage (blankImage 5 5))
  flush
  
idle :: IORef Int -> IdleCallback
idle iter = do
  iter $~! (+ 1)
  postRedisplay Nothing

{-
display :: DisplayCallback
display = do
  clear [ ColorBuffer ]       -- clears canvas?
  draw (gradientImage (blankImage 500 500))  -- Converts points to an image 500 x 500 (points must be in range 0 - 500)
  flush                       -- Sends openGL commands to graphics for display
-}

draw :: Image -> IO ()
draw (Image points w h) = do
                            renderPrimitive Points $
                                mapM_ (\point -> drawSingle point w h) points
                
drawSingle :: MyPixel -> Int -> Int -> IO ()
drawSingle (MyPixel (MyPoint x y) (MyColor r g b o)) w h = do
                                                            let
                                                                scaleColor c = (fromIntegral (c :: Int) :: Double) / 256
                                                                toFloat v = (fromIntegral (v :: Int) :: Float)
                                                            color  $ Color4 (scaleColor r)
                                                                            (scaleColor g)
                                                                            (scaleColor b)
                                                                            o
                                                            vertex $ Vertex2 (((toFloat x) / (toFloat w) * 2) - 1)
                                                                             (((toFloat y) / (toFloat h) * 2) - 1)



                                                            
                                                            
                                                            
                                                            
{-----------------------------   Library Functions   -------------------------}

blankImage :: Int -> Int -> Image
blankImage w h = Image (map f points) w h
                   where
                        f = (\point -> MyPixel point (MyColor 256 256 256 1))
                        points = [(MyPoint x y) | x <- [1..w], y <- [1..h] ]

gradientImage :: Image -> Image
gradientImage (Image pixels w h) = Image (map (\p -> changePixel p) pixels) w h
                                    where
                                        changePixel (MyPixel (MyPoint x y) _) = MyPixel (MyPoint x y)
                                                                                        (MyColor (round (((toFloat x) / (toFloat w)) * 256))
                                                                                                 (round (((toFloat y) / (toFloat h)) * 256))
                                                                                                 128    
                                                                                                 1)
                                        toFloat v = (fromIntegral (v :: Int) :: Float)







{----------------------------     Data and Types --------------------------------}

data MyPoint = MyPoint Int Int
data MyColor = MyColor { red     :: Int 
                       , green   :: Int 
                       , blue    :: Int
                       , opacity :: Double }
    
data MyPixel = MyPixel MyPoint MyColor

instance Show MyColor where
    show (MyColor r g b o) = "(" ++ show r ++ 
                             ", " ++ show g ++ 
                             ", " ++ show b ++ 
                             ", " ++ show o ++ ")"
    
    
{----------------------------    Smart Constructors    -----------------------------}

mkPixel :: Int -> Int -> Int -> Int -> Int -> Double -> MyPixel
mkPixel x y r g b o = MyPixel (MyPoint x y) (mkColor r g b o)
        
mkColor :: Int -> Int -> Int -> Double -> MyColor
mkColor r g b o = assert (r >= 0 && g >= 0 && b >= 0 &&
                          r < 256 && g < 256 && b < 256 &&
                          o >= 0 && o <= 1)
                         $ MyColor r g b o

                         
                         
                         
{----------------------------    Images and Animations     ---------------------------}

-- Image contains a list of MyPixels
-- and an integer describing its width
-- and one for its height
data Image = Image [MyPixel] Int Int 

imageMap :: (MyPixel -> MyPixel) -> Image -> Image
imageMap f (Image pxls h w) = Image (map f pxls) h w


-- Maintains speed of animation as number of loops per frame
data Animation = Animation [Image] Int

animationMap :: (Image -> Image) -> Animation -> Animation
animationMap f (Animation images loops) = Animation (map f images) loops

















{-


--- Helper function applications

applyRed :: (Int -> Int) -> MyPixel -> MyPixel
applyRed f (MyPixel r g b o) = MyPixel (f r) g b o

applyGreen :: (Int -> Int) -> MyPixel -> MyPixel
applyGreen f (MyPixel r g b o) = MyPixel r (f g) b o

applyBlue :: (Int -> Int) -> MyPixel -> MyPixel
applyBlue f (MyPixel r g b o) = MyPixel r g (f b) o

applyOpac :: (Double -> Double) -> MyPixel -> MyPixel
applyOpac f (MyPixel r g b o) = MyPixel r g b (f o)



--- Helper MyPixel mappings onto images

redMap :: (Int -> Int) -> Image -> Image
redMap f = imageMap (\c -> applyRed f c)

greenMap :: (Int -> Int) -> Image -> Image
greenMap f = imageMap (\c -> applyGreen f c)

blueMap :: (Int -> Int) -> Image -> Image
blueMap f = imageMap (\c -> applyBlue f c)
 
opacMap :: (Double -> Double) -> Image -> Image
opacMap f = imageMap (\c -> applyOpac f c)



--- Shift MyPixels

shiftMyPixel :: Int -> Int -> Int
shiftMyPixel val change
                    | res > 255 = 255
                    | res < 0   = 0
                    | otherwise = res
                    where
                        res = val + change

shiftOpac :: Double -> Double -> Double
shiftOpac val change
                    | res >= 1  = 1
                    | res <= 0  = 0
                    | otherwise = res
                    where
                        res = val + change

-- Shifting example
    
plusFiveRed :: Image -> Image
plusFiveRed = redMap (\v -> shiftMyPixel 5 v)








-}
























