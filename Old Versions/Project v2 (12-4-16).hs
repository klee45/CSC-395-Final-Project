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

exampleRaw    = fromPng "Sprites/MarioSmall_1.png"
backgroundRaw = fromPng "Sprites/BackgroundSmall.png"

exF1 img = imageReplaceColor 50 (MyColor 184 64 64 1) (MyColor 0 255 0 1) img
exF2 img = imageMap (\c -> colorAdd c (-20) 20 0) img
exF3 img = imageInvert img

example    = toDrawable (do
                            img <- exampleRaw
                            return (exF3 (exF2 (exF1 img))))
                        0 0 0.15 0.2
background = toDrawable backgroundRaw 0 0 1.0 1.0

{----------------------------------  Main and Display ----------------------------}

-- Main Function
-- Yeah I have no idea what any of this does

main :: IO ()
main = do
          (_progName, _args) <- getArgsAndInitialize
          initialWindowSize $= Size 640 480
          _window <- createWindow "Test"
          blend $= Enabled
          blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
          viewport $= (Position 0 0, Size (fromIntegral 100) (fromIntegral 100))
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
  i1 <- background
  i2 <- example
  draw i1
  draw i2
  flush                       -- Sends openGL commands to graphics for display

draw :: Drawable -> IO ()
draw (Drawable triplet w h) = do
                                mapM_ (\(pixelColor, v1, v2) -> do
                                                                    color pixelColor
                                                                    rect v1 v2)
                                      triplet

drawBigPixel :: (Color4 Double, Vertex2 Double, Vertex2 Double) -> IO ()
drawBigPixel (pixelColor, v1, v2) = do
                                    color pixelColor
                                    rect v1 v2
                 
drawCanvas :: IO ()
drawCanvas = do
                let -- Just pretend this isn't here
                    n :: Double
                    n = -1.0;
                    p :: Double
                    p = 1.0
                color (Color4 1.0 1.0 1.0 1.0 :: Color4 Double)
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

mkColorBounded :: Int -> Int -> Int -> Double -> MyColor
mkColorBounded r g b a = mkColor (bound r) (bound g) (bound b) (bounda a)
    where
        bound i
            | i < 0     = 0
            | i > 255   = 255
            | otherwise = i
        bounda a
            | a < 0.0   = 0.0
            | a > 1.0   = 1.0
            | otherwise = a
                         
{----------------------------    Images and Animations     ---------------------------}

-- MyImage contains a list of MyPixels
-- and an integer describing its width
-- and one for its height
data MyImage = MyImage [(MyColor, MyPoint)] Int Int 

imageMap :: (MyColor -> MyColor) -> MyImage -> MyImage
imageMap f (MyImage pairs w h) = MyImage (map (\(color, point) -> ((f color), point)) pairs) w h


-- Maintains speed of animation as number of loops per frame
data Animation = Animation [Drawable] Int

getFrame :: Animation -> Int -> Drawable
getFrame (Animation imgs _) frame = (!!) imgs (frame `mod` (length imgs))

animationMap :: (Drawable -> Drawable) -> Animation -> Animation
animationMap f (Animation images frames) = Animation (map f images) frames


{------------------------------ Drawable --------------------------------------------------}
{-
    Pre-processing for faster drawing. Definition is nearly identical to MyImage, 
    but with a data declaration and a hidden constructor, we can ensure that images are
    pre-processed before drawing.
    
    Also this looks really really ugly and I think some parts aren't necessary
-}
data Drawable = Drawable [(Color4 Double, Vertex2 Double, Vertex2 Double)] Int Int

toDrawable :: IO MyImage -> Int -> Int -> Double -> Double -> IO Drawable
toDrawable ioImg x y w h = do
                            res <- ioImg
                            let (MyImage pairs imgW imgH) = res
                                toDouble  v = (fromIntegral (v :: Int) :: Double)
                                pixelWidth  = 2.0 / (toDouble imgW) * w
                                pixelHeight = 2.0 / (toDouble imgH) * h
                                scaleColor c = (toDouble c) / 256
                                helper ((MyColor r g b a), (MyPoint px py)) = (newColor, Vertex2 x1 y1, Vertex2 x2 y2)    
                                    where
                                        x1 = (((toDouble (px + x)) / (toDouble imgW) * 2 * w) - 1)
                                        x2 = x1 + pixelWidth
                                        -- For some reason the juicypixels people made it so the top left
                                        -- is the origin instead of the standard of having the bottom left
                                        -- It could be backwards I'm too lazy to check.
                                        y1 = (((toDouble (imgH - py + y)) / (toDouble imgH) * 2 * h) - 1)
                                        y2 = y1 - pixelHeight
                                        newColor = (Color4 (scaleColor r)
                                                           (scaleColor g)
                                                           (scaleColor b)
                                                           a)
                            return $ Drawable (map helper pairs) imgW imgH
            
{-------------------------------------------- Functions ------------------------------------------------}

{-------------------------------------------- Image Functions ------------------------------------------}


-- Split horizontally
-- If n doesn't make sense or the image can't be split,
-- returns the same image
splitH :: MyImage -> Int -> [MyImage]
splitH image@(MyImage pairs w h) n
    | n <= 1           = [image]
    | (w `mod` n) /= 0 = [image] -- Why 'not equals' is written as '/=' in haskell makes no sense
    | otherwise        = undefined
    where
        temp = [[0..(w `div` n)]]

-- Split Vertically
splitV :: MyImage -> Int -> [MyImage]
splitV i 0 = [i]
splitV i 1 = [i]
splitV image@(MyImage pairs w h) n
    | n <= 1           = [image]
    | (h `mod` n) /= 0 = [image]
    | otherwise        = undefined
    where
        temp = [[0..(h `div` n)]]

        
        

imageReplaceColor :: Int -> MyColor -> MyColor -> MyImage -> MyImage
imageReplaceColor m compare@(MyColor r' g' b' _) new image = imageMap helper image
    where
        helper old@(MyColor r g b a)
            | withinR && withinG && withinB = new
            | otherwise                     = old 
            where
                withinR = (abs (r' - r)) <= m
                withinG = (abs (g' - g)) <= m
                withinB = (abs (b' - b)) <= m

imageInvert :: MyImage -> MyImage
imageInvert image = imageMap (\(MyColor r g b a) -> mkColorBounded (255 - r) (255 - g) (255 - b) a) image

imageGrayscale :: MyImage -> MyImage
imageGrayscale image = imageMap helper image
    where
        helper (MyColor r g b a) = mkColorBounded l l l a
            where
                l = round ((times 0.2126 r) + (times 0.7152 g) + (times 0.0722 b))
                times dbl v = dbl * (fromIntegral (v :: Int) :: Double)

{------------------------------------------------ Individual Color Functions -----------------------------}


colorAdd :: MyColor -> Int -> Int -> Int -> MyColor
colorAdd (MyColor r g b a) rp gp bp = mkColorBounded (r + rp) (g + gp) (b + bp) a


























