{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Graphics.UI.GLUT
import Data.IORef
import Codec.Picture.Saving
import qualified Codec.Picture as J
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import Data.Word


{-- TODO

Finish conversion from our images to pngs through juicypixels

Remove / rename pixel to remove x y location and instead fill entire rect
    or edit conversion to fill remaining elements with transparent

-}

{-
    File reading, writing, and helpers
-}

fromPng :: FilePath -> IO MyImage
fromPng path = do
                let
                    makeImage :: J.DynamicImage -> MyImage
                    makeImage (J.ImageRGBA8 image@(J.Image w h _)) = convertPixels image w h
                    makeImage _ = blankImage 1 1
                    convertPixels pixels w h = MyImage (zip (map (\(MyPoint x y) -> pixelToColor (J.pixelAt pixels x y)) points) points) w h
                                            where
                                                points = [(MyPoint x y) | x <- [0..w-1], y <- [0..h-1]]
                    pixelToColor ::  J.PixelRGBA8 -> MyColor
                    pixelToColor (J.PixelRGBA8 r g b a) = MyColor (toInt r)
                                                                  (toInt g)
                                                                  (toInt b)
                                                                  (alphaConvert a)
                    toInt w = (fromIntegral (w :: Word8) :: Int)
                    alphaConvert w = (fromIntegral (w :: Word8) :: Double) / 256

                file <- B.readFile path                 -- file   :: ByteString
                let (Right result) = J.decodePng file   -- result :: DynamicImage
                return (makeImage result)




                
{--------------------------- Loading some examples -------------------------------}

example = fromPng "Sprites/MarioSmall.png"

displayAlpha :: IO MyImage -> IO ()
displayAlpha ioimg = undefined



{----------------------------------  Main and Display ----------------------------}

-- Main Function
-- Yeah I have no idea what any of this does

main :: IO ()
main = do
          (_progName, _args) <- getArgsAndInitialize
          initialDisplayMode $= [RGBAMode, 
                                 WithAlphaComponent]
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
  img <- example
  drawCanvas
  draw (blueRotate img a)  -- Converts points to an MyImage 500 x 500 (points must be in range 0 - 500)
  flush                       -- Sends openGL commands to graphics for display

draw :: MyImage -> IO ()
draw (MyImage pixels w h) = do
                                renderPrimitive Quads $
                                    mapM_ (\point -> drawBigPixel point w h) pixels

                                    
{-  Currently I have no idea how we can get alpha working
    the workaround is to not display the elements with
    0 alpha
-}    
drawBigPixel :: (MyColor, MyPoint) -> Int -> Int -> IO ()
drawBigPixel ((MyColor _ _ _ 0), _) w h = return ()
drawBigPixel ((MyColor r g b a),(MyPoint x y))
             w
             h = do
                    let
                        pixelWidth  = 2.0 / (toFloat w)
                        pixelHeight = 2.0 / (toFloat h)
                        scaleColor c = (fromIntegral (c :: Int) :: GLdouble) / 256
                        toFloat    v = (fromIntegral (v :: Int) :: GLdouble)
                        x1 = (((toFloat x) / (toFloat w) * 2) - 1)
                        x2 = x1 + pixelWidth
                        -- For some reason the juicypixels people made it so the top left
                        -- is the origin instead of the standard of having the bottom left
                        -- It could be backwards I'm too lazy to check.
                        y1 = (((toFloat (h - y)) / (toFloat h) * 2) - 1)
                        y2 = y1 - pixelHeight
                    color (Color4 (scaleColor r)
                                  (scaleColor g)
                                  (scaleColor b)
                                  a :: Color4 GLdouble)
                    vertex $ Vertex2 x1 y1
                    vertex $ Vertex2 x2 y1
                    vertex $ Vertex2 x2 y2
                    vertex $ Vertex2 x1 y2
                                                                 
drawCanvas :: IO ()
drawCanvas = do
                let -- Just pretend this isn't here
                    n :: GLdouble
                    n = -1.0;
                    p :: GLdouble
                    p = 1.0
                color (Color4 1.0 1.0 1.0 1.0 :: Color4 GLdouble)
                renderPrimitive Quads $ mapM_ vertex [
                    Vertex2 n n,
                    Vertex2 p n,
                    Vertex2 p p,
                    Vertex2 n p]                                      
                                                                             
blueRotate :: MyImage -> Int -> MyImage
blueRotate (MyImage pixels w h) v = MyImage (map (\((MyColor r g b a), point) -> ((MyColor r g (rotate b v) a), point)) pixels)
                                       w h
            where
                rotate b v = (b + (v * 2)) `mod` 256 

                                                            
{-                     
data Displayable = Displayable [(MyColor, MyPoint)] Int Int

toDisplayable :: MyImage -> Displayable
toDisplayable (MyImage colors w h) = Displayable (zip colors 
                                                      [MyPoint x y | x <- [0..(w-1)], y <- [0..(h-1)]])
                                                  w h
-}                                 
                                                            
{-----------------------------   Library Functions   -------------------------}

blankImage :: Int -> Int -> MyImage
blankImage w h = MyImage [((MyColor 256 256 256 1), (MyPoint x y)) | x <- [0..(w-1)] , y <- [0..(h-1)]] w h

gradientImage :: MyImage -> MyImage
gradientImage (MyImage pixels w h) = MyImage (map (\p -> changePixel p) pixels) w h
              where
                changePixel (_, point@(MyPoint x y)) = ((MyColor (round (((toFloat x) / (toFloat w)) * 256))
                                                                 (round (((toFloat y) / (toFloat h)) * 256))
                                                                 128    
                                                                 1),
                                                        point)
                toFloat v = (fromIntegral (v :: Int) :: Float)







{----------------------------     Data and Types --------------------------------}

data MyPoint = MyPoint Int Int
data MyColor = MyColor { red     :: Int 
                       , green   :: Int 
                       , blue    :: Int
                       , opacity :: Double }

instance Show MyColor where
    show (MyColor r g b a) = "("  ++ show r ++ 
                             ", " ++ show g ++ 
                             ", " ++ show b ++ 
                             ", " ++ show a ++ ")"
    
    
{----------------------------    Smart Constructors    -----------------------------}
        
mkColor :: Int -> Int -> Int -> Double -> MyColor
mkColor r g b a = assert (r >= 0 && g >= 0 && b >= 0 &&
                          r < 256 && g < 256 && b < 256 &&
                          a >= 0 && a <= 1)
                         $ MyColor r g b a

                         
                         
                         
{----------------------------    Images and Animations     ---------------------------}

-- MyImage contains a list of MyPixels
-- and an integer describing its width
-- and one for its height
data MyImage = MyImage [(MyColor, MyPoint)] Int Int 

imageMap :: (MyColor -> MyColor) -> MyImage -> MyImage
imageMap f (MyImage pairs w h) = MyImage (map (\(color, point) -> ((f color), point)) pairs) w h


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
























