{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Graphics.UI.GLUT
import Data.IORef
import Codec.Picture.Saving
import qualified Codec.Picture as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V



{-- TODO

Finish conversion from our images to pngs through juicypixels

Remove / rename pixel to remove x y location and instead fill entire rect
    or edit conversion to fill remaining elements with transparent

--}

{---------------------------   Convert to Codec Image and PNG   -------------------}

convertToPng :: MyImage -> B.ByteString
convertToPng image = J.encodePng (toJImage image)
    where
        toJImage :: MyImage -> J.Image J.PixelRGBA8 -- ImageRGBA (Image PixelRGBA8)
        toJImage (MyImage pixels w h) = J.Image w h (V.fromList (map toJColor pixels))
        toJColor :: MyColor -> J.PixelRGBA8 -- (even A layer is 0-255)
        toJColor (MyColor r g b a) = undefined



{----------------------------------  Main and Display ----------------------------}

-- Main Function
-- Yeah I have no idea what any of this does

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Test"
  iter <- newIORef 1
  displayCallback $= display iter
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle iter)
  mainLoop
  
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
  
idle :: IORef Int -> IdleCallback
idle iter = do
  iter $~! (+ 1)
  postRedisplay Nothing

display :: IORef Int -> DisplayCallback
display iter = do
  clear [ ColorBuffer ]       -- clears canvas?
  a <- get iter
  draw (blueRotate (gradientImage (blankImage 25 25)) a)  -- Converts points to an MyImage 500 x 500 (points must be in range 0 - 500)
  flush                       -- Sends openGL commands to graphics for display

draw :: MyImage -> IO ()
draw (MyImage points w h) = do
                            renderPrimitive Quads $
                                mapM_ (\point -> drawBigPixel point w h) points
                                
pixelSize = 0.05
                
drawSingle :: MyPixel -> Int -> Int -> IO ()
drawSingle (MyPixel (MyPoint x y) (MyColor r g b a)) w h = do
                                                            let
                                                                scaleColor c = (fromIntegral (c :: Int) :: Double) / 256
                                                                toFloat v = (fromIntegral (v :: Int) :: Float)
                                                            color  $ Color4 (scaleColor r)
                                                                            (scaleColor g)
                                                                            (scaleColor b)
                                                                            a
                                                            vertex $ Vertex2 (((toFloat x) / (toFloat w) * 2) - 1)
                                                                             (((toFloat y) / (toFloat h) * 2) - 1)
                                                                             
drawBigPixel :: MyPixel -> Int -> Int -> IO ()
drawBigPixel (MyPixel (MyPoint x y)
                      (MyColor r g b a))
                      w
                      h = do
                            let
                                scaleColor c = (fromIntegral (c :: Int) :: Double) / 256
                                toFloat    v = (fromIntegral (v :: Int) :: Float)
                                x1 = (((toFloat x) / (toFloat w) * 2) - 1)
                                x2 = x1 + pixelSize
                                y1 = (((toFloat y) / (toFloat h) * 2) - 1)
                                y2 = y1 + pixelSize
                            color  $ Color4 (scaleColor r)
                                            (scaleColor g)
                                            (scaleColor b)
                                            a
                            vertex $ Vertex2 x1 y1
                            vertex $ Vertex2 x2 y1
                            vertex $ Vertex2 x2 y2
                            vertex $ Vertex2 x1 y2
                                                 
                                
                                                                                
                                                                             
blueRotate :: MyImage -> Int -> MyImage
blueRotate (MyImage pixels w h) v = MyImage (map (\(MyPixel pnt (MyColor r g b a)) -> 
                                            MyPixel pnt (MyColor r g (rotate b v) a)) pixels)
                                       w h
            where
                rotate b v = (b + (v * 2)) `mod` 256 

                                                            
                                                            
                                                            
                                                            
{-----------------------------   Library Functions   -------------------------}

blankImage :: Int -> Int -> MyImage
blankImage w h = MyImage (map f points) w h
                   where
                        f = (\point -> MyPixel point (MyColor 256 256 256 1))
                        points = [(MyPoint x y) | x <- [1..w], y <- [1..h] ]

gradientImage :: MyImage -> MyImage
gradientImage (MyImage pixels w h) = MyImage (map (\p -> changePixel p) pixels) w h
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
    show (MyColor r g b a) = "("  ++ show r ++ 
                             ", " ++ show g ++ 
                             ", " ++ show b ++ 
                             ", " ++ show a ++ ")"
    
    
{----------------------------    Smart Constructors    -----------------------------}

mkPixel :: Int -> Int -> Int -> Int -> Int -> Double -> MyPixel
mkPixel x y r g b a = MyPixel (MyPoint x y) (mkColor r g b a)
        
mkColor :: Int -> Int -> Int -> Double -> MyColor
mkColor r g b a = assert (r >= 0 && g >= 0 && b >= 0 &&
                          r < 256 && g < 256 && b < 256 &&
                          a >= 0 && a <= 1)
                         $ MyColor r g b a

                         
                         
                         
{----------------------------    Images and Animations     ---------------------------}

-- MyImage contains a list of MyPixels
-- and an integer describing its width
-- and one for its height
data MyImage = MyImage [MyPixel] Int Int 

imageMap :: (MyPixel -> MyPixel) -> MyImage -> MyImage
imageMap f (MyImage pxls h w) = MyImage (map f pxls) h w


-- Maintains speed of animation as number of loops per frame
data Animation = Animation [MyImage] Int

getFrame :: Animation -> Int -> MyImage
getFrame (Animation imgs _) frame = (!!) imgs (frame `mod` (length imgs))

animationMap :: (MyImage -> MyImage) -> Animation -> Animation
animationMap f (Animation images frames) = Animation (map f images) frames

















{-


--- Helper function applications

applyRed :: (Int -> Int) -> MyPixel -> MyPixel
applyRed f (MyPixel r g b a) = MyPixel (f r) g b a

applyGreen :: (Int -> Int) -> MyPixel -> MyPixel
applyGreen f (MyPixel r g b a) = MyPixel r (f g) b a

applyBlue :: (Int -> Int) -> MyPixel -> MyPixel
applyBlue f (MyPixel r g b a) = MyPixel r g (f b) a

applyOpac :: (Double -> Double) -> MyPixel -> MyPixel
applyOpac f (MyPixel r g b a) = MyPixel r g b (f a)



--- Helper MyPixel mappings onto images

redMap :: (Int -> Int) -> MyImage -> MyImage
redMap f = imageMap (\c -> applyRed f c)

greenMap :: (Int -> Int) -> MyImage -> MyImage
greenMap f = imageMap (\c -> applyGreen f c)

blueMap :: (Int -> Int) -> MyImage -> MyImage
blueMap f = imageMap (\c -> applyBlue f c)
 
opacMap :: (Double -> Double) -> MyImage -> MyImage
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
    
plusFiveRed :: MyImage -> MyImage
plusFiveRed = redMap (\v -> shiftMyPixel 5 v)








-}
























