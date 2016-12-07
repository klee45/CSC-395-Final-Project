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
import qualified Data.Matrix as M


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
                    convertPixels pixels w h = MyImage (M.fromList w h (map (\(MyPoint x y) -> pixelToColor (J.pixelAt pixels x y)) points))
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

exampleRaw1   = fromPng "Sprites/MarioSmall_1.png"
exampleRaw2   = fromPng "Sprites/MarioBig_1.png"
backgroundRaw = fromPng "Sprites/BackgroundSmall.png"

exF1 = imageReplaceColor 50 (MyColor 184 64 64 1) (MyColor 0 255 0 1)
exF2 = imageMap (\c -> colorAdd c (-20) 20 0)
exF3 = imageInvert

exA1 ani = (animationBlink ani 9)

example1    = do 
                img <- exampleRaw1
                return $ toDrawable (exF3 (exF2 (exF1 img)))
                                    0 0 0.15 0.2
                                    
example2    = do
                img <- exampleRaw2
                return $ mkAnimation (animationBlink img 17)
                                     15 0 0.40 0.2
                                    
background = do img <- backgroundRaw 
                return $ toDrawable img 0 0 1.0 1.0

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
  i2 <- example1
  a1 <- example2
  --draw i1
  draw i2
  drawAnimation a1 a
  flush                       -- Sends openGL commands to graphics for display

draw :: Drawable -> IO ()
draw (Drawable triplet w h) = do
                                mapM_ (\(pixelColor, v1, v2) -> do
                                                                    color pixelColor
                                                                    rect v1 v2)
                                      triplet
                                      
drawAnimation :: Animation -> Int -> IO ()
drawAnimation animation frame = draw (getFrame animation frame)

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
                          
{-                          
blueRotate :: MyImage -> Int -> MyImage
blueRotate (MyImage pixels w h) v = MyImage (map (\((MyColor r g b a), point) -> ((MyColor r g (rotate b v) a), point)) pixels) w h
            where
                rotate b v = (b + (v * 2)) `mod` 256 
-}                          
                                                            
{-----------------------------   Library Functions   -------------------------}

blankImage :: Int -> Int -> MyImage
blankImage w h = MyImage $ M.fromList w h (replicate (w * h) (MyColor 256 256 256 1))

{-
gradientImage :: MyImage -> MyImage
gradientImage (MyImage pixels w h) = MyImage (map (\p -> changePixel p) pixels) w h
              where
                changePixel (_, point@(MyPoint x y)) = ((MyColor (round (((toFloat x) / (toFloat w)) * 256))
                                                                 (round (((toFloat y) / (toFloat h)) * 256))
                                                                 128    
                                                                 1),
                                                        point)
                toFloat v = (fromIntegral (v :: Int) :: Float)
-}






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
data MyImage = MyImage (M.Matrix MyColor)

imageMap :: (MyColor -> MyColor) -> MyImage -> MyImage
imageMap f (MyImage matrix) = MyImage (matrixMap f matrix)

imageMapXY :: (MyColor -> Int -> Int -> MyColor) -> MyImage -> MyImage
imageMapXY f (MyImage matrix) = MyImage (matrixMapXY f matrix);

matrixMap :: (a -> b) -> M.Matrix a -> M.Matrix b
matrixMap f m = M.fromList cols rows (map f (M.toList m))
    where
        cols = M.ncols m
        rows = M.nrows m
            
matrixMapXY :: (a -> Int -> Int -> b) -> M.Matrix a -> M.Matrix b            
matrixMapXY f m = M.fromList cols rows (map helper (zip (M.toList m) grid))
    where
        grid = [(x,y) | x <- [1..(M.ncols m)], y <- [1..(M.nrows m)]]
        helper (v, (x, y)) = f v x y
        cols = M.ncols m
        rows = M.nrows m

-- Maintains speed of animation as number of loops per frame
data Animation = Animation [Drawable] Int

getFrame :: Animation -> Int -> Drawable
getFrame (Animation imgs _) frame = (!!) imgs (frame `mod` (length imgs))

animationMap :: (Drawable -> Drawable) -> Animation -> Animation
animationMap f (Animation images frames) = Animation (map f images) frames

mkAnimation :: [MyImage] -> Int -> Int -> Double -> Double -> Animation
mkAnimation images x y w h = Animation (map (\i -> (toDrawable i x y h w)) images) (length images)

{------------------------------ Drawable --------------------------------------------------}
{-
    Pre-processing for faster drawing. Definition is nearly identical to MyImage, 
    but with a data declaration and a hidden constructor, we can ensure that images are
    pre-processed before drawing.
    
    Also this looks really really ugly and I think some parts aren't necessary
-}
data Drawable = Drawable [(Color4 Double, Vertex2 Double, Vertex2 Double)] Int Int

toDrawable :: MyImage -> Int -> Int -> Double -> Double -> Drawable
toDrawable (MyImage matrix) x y w h = Drawable (M.toList (matrixMapXY helper matrix)) cols rows
    where
        cols = M.ncols matrix
        rows = M.nrows matrix
        toDouble  v = (fromIntegral (v :: Int) :: Double)
        pixelWidth  = 2.0 / (toDouble cols) * w
        pixelHeight = 2.0 / (toDouble rows) * h
        scaleColor c = (toDouble c) / 256
        helper (MyColor r g b a) px py = (newColor, Vertex2 x1 y1, Vertex2 x2 y2)
            where
                x1 = (((toDouble (px + x)) / (toDouble cols) * 2 * w) - 1)
                x2 = x1 + pixelWidth
                -- For some reason the juicypixels people made it so the top left
                -- is the origin instead of the standard of having the bottom left
                -- It could be backwards I'm too lazy to check.
                y1 = (((toDouble (rows - py + y)) / (toDouble rows) * 2 * h) - 1)
                y2 = y1 - pixelHeight
                newColor = (Color4 (scaleColor r)
                                   (scaleColor g)
                                   (scaleColor b)
                                   a)
                                   
{-------------------------------------------- Functions ------------------------------------------------}

{-------------------------------------------- Image Functions ------------------------------------------}

{-

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
    
-}      

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
imageInvert image = imageMap (\(MyColor r g b a) -> mkColorBounded (255 - r)
                                                                   (255 - g)
                                                                   (255 - b)
                                                                   a)
                             image

imageGrayscale :: MyImage -> MyImage
imageGrayscale image = imageMap helper matrix
    where
        helper (MyColor r g b a) = mkColorBounded l l l a
            where
                l = round ((times 0.2126 r) + (times 0.7152 g) + (times 0.0722 b))
                times dbl v = dbl * (fromIntegral (v :: Int) :: Double)

{--------------------------------------- Individual Color Functions --------------------------------------}

colorAdd :: MyColor -> Int -> Int -> Int -> MyColor
colorAdd (MyColor r g b a) rp gp bp = mkColorBounded (r + rp) (g + gp) (b + bp) a

{--------------------------------------- Animation Functions ---------------------------------------------}
{-
        Animation functions return a list of images rather than an actual
    animation type. This information needs to be converted to an animation
    by passing it through pre-processing
-}

-- This function probably needs to take
-- an odd number of frames as you go to
-- white and return
--
-- Additionally, frames must be >= 3
animationBlink :: MyImage -> Int -> [MyImage]
animationBlink image frames = map helper ([0..frames] ++ [frames-1,frames-2..0])
    where
        helper frame = imageMap (\c -> colorToWhite c frame) image
        colorToWhite (MyColor r g b a) frame = (MyColor (whiten r ratio) (whiten g ratio) (whiten b ratio) a)
            where
                ratio = (toDouble frame) / (toDouble frames)
        whiten v ratio = floor ((toDouble v) + (toDouble (255 - v)) * ratio)
        toDouble v = (fromIntegral (v :: Int) :: Double)





















