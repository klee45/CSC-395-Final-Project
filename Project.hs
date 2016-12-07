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

Exporting

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
                    convertPixels pixels w h = MyImage (M.fromList h w (map (\(MyPoint x y) -> pixelToColor (J.pixelAt pixels x y)) points))
                                            where
                                                points = [(MyPoint x y) | y <- [0..h-1], x <- [0..w-1]]
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


                
{---------------------------------------------------------------------------------}

exColorTransparent = (MyColor 0 0 0 0)
exColorWhite       = (MyColor 256 256 256 1)

                
{--------------------------- Loading some examples -------------------------------}

exampleRaw1   = fromPng "Sprites/MarioSmall_1.png"
exampleRaw2   = fromPng "Sprites/MarioBig_1.png"
backgroundRaw = fromPng "Sprites/BackgroundSmall.png"

exF1 = imageReplaceColor 50 (MyColor 184 64 64 1) (MyColor 0 255 0 1)
exF2 = imageMap (\c -> colorAdd c (-20) 20 0)
exF3 = imageInvert

-- Expand image 5 pixels on all sides with transparent pixels
exF4 = imageExpandBorders exColorTransparent 5 5 5 5
exF5 = imageGetOutline 2

exA1 = animationBlink (MyColor 256 256 256 1) 300

exA2 = animationGlow [(MyColor 255 255 50  1),
                      (MyColor 255 150 25   1)]
                     [(MyColor 255 150 25   1),
                      (MyColor 255 0   0   1)]
                     17

example1 = do 
            img <- exampleRaw1
            return $ toDrawable (exF5 (exF4 (exF3 (exF2 (exF1 img)))))
                                5 5 0.25 0.30
                                     
example2 = do
            img <- exampleRaw2
            return $ mkAnimation (exA1 (exF4 img))
                                 25 5 0.6 0.4
                      
example3 = do
            img <- exampleRaw2
            return $ mkAnimation (exA2 (exF4 img))
                                 25 5 0.6 0.4
                                     
example4 = do
            img <- exampleRaw2
            let (MyImage matrix) = img
            return $ toDrawable (imageOverlay (blankImage (M.ncols matrix) (M.nrows matrix)) img)
                                10 10 0.3 0.3        
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
  i3 <- example4

  a1 <- example2
  a2 <- example3
  
  --drawCanvas
  --draw i1
  draw i2
  --draw i3
  drawAnimation a1 a
  drawAnimation a2 a
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
blankImage w h = emptyImage w h $ exColorWhite

transparentImage :: Int -> Int -> MyImage
transparentImage w h = emptyImage w h $ exColorTransparent

emptyImage :: Int -> Int -> MyColor -> MyImage
emptyImage w h color = MyImage $ M.fromList h w (replicate (w * h) color)

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
                        







                        
{----------------------------    Images and Matrices     ---------------------------}

-- MyImage contains a list of MyPixels
-- and an integer describing its width
-- and one for its height
data MyImage = MyImage (M.Matrix MyColor)

imageMap :: (MyColor -> MyColor) -> MyImage -> MyImage
imageMap f (MyImage matrix) = MyImage (matrixMap f matrix)

imageMapXY :: (MyColor -> Int -> Int -> MyColor) -> MyImage -> MyImage
imageMapXY f (MyImage matrix) = MyImage (matrixMapXY f matrix);

matrixMap :: (a -> b) -> M.Matrix a -> M.Matrix b
matrixMap f m = M.fromList rows cols (map f (M.toList m))
    where
        cols = M.ncols m
        rows = M.nrows m
            
matrixMapXY :: (a -> Int -> Int -> b) -> M.Matrix a -> M.Matrix b            
matrixMapXY f m = M.fromList rows cols (map helper (zip (M.toList m) grid))
    where
        grid = [(x,y) | y <- [1..rows], x <- [1..cols]]
        helper (v, (x, y)) = f v x y
        cols = M.ncols m
        rows = M.nrows m

imageExpandBorders :: MyColor -> Int -> Int -> Int -> Int -> MyImage -> MyImage
imageExpandBorders replace left right up down (MyImage old) = MyImage new
    where
        rows = M.nrows old
        cols = M.ncols old
        s1 = M.extendTo replace           -- Extend right and bottom border
                        (down + rows)
                        (right + cols)
                        old
        s2 = flip s1                      -- Reverse rows, transpose, reverse cols
        s3 = M.extendTo replace
                        (up + rows + down)
                        (left + cols + right)
                        s2
        new = flip s3 
        rev = map reverse . M.toLists
        tra = M.transpose . M.fromLists
        flip = tra . rev . tra . rev

        
        
        
        
{------------------------------ Animation --------------------------------------------------}
        
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
                -- Not actually magic numbers
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
                                 



                                 
{-******************* Functions ***********************************************************************-}

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
        helper old@(MyColor _ _ _ 0)        = old
        helper old@(MyColor r g b a)
            | withinR && withinG && withinB = new
            | otherwise                     = old 
            where
                withinR = (abs (r' - r)) <= m
                withinG = (abs (g' - g)) <= m
                withinB = (abs (b' - b)) <= m
                
imageReplaceAllColor :: MyColor -> MyImage -> MyImage
imageReplaceAllColor color = imageMap helper
    where
        helper old@(MyColor _ _ _ 0) = old
        helper _                     = color

imageInvert :: MyImage -> MyImage
imageInvert image = imageMap (\(MyColor r g b a) -> mkColorBounded (255 - r)
                                                                   (255 - g)
                                                                   (255 - b)
                                                                   a)
                             image

imageGrayscale :: MyImage -> MyImage
imageGrayscale image = imageMap helper image
    where
        helper (MyColor r g b a) = mkColorBounded l l l a
            where
                l = round ((times 0.2126 r) + (times 0.7152 g) + (times 0.0722 b))
                times dbl v = dbl * (fromIntegral (v :: Int) :: Double)
                
{-
    Extremely simple edge detection algorithm
    Simply checks if a pixel both
        1. is transparent (alpha 0)
        2. has a nontransparent neighbor
           within the given range
           
    Returns a white pixelmap
-}
imageGetOutline :: Int -> MyImage -> MyImage
imageGetOutline range (MyImage matrix) = MyImage (matrixMapXY checkRange matrix)
    where
        cols = M.ncols matrix
        rows = M.nrows matrix
        checkRange (MyColor _ _ _ 0) x y
            | foldl (||) False (getResults x y)  = exColorWhite
            | otherwise                          = exColorTransparent
        checkRange _ _ _                         = exColorTransparent
        getResults x y = map checkPixel [(x',y') | x' <- [(x - range)..(x + range)],
                                                   y' <- [(y - range)..(y + range)]]
            where
                checkPixel (x',y')
                    | (x' < 1)    ||
                      (x' > cols) ||
                      (y' < 1)    ||
                      (y' > rows) = False
                    | otherwise                 = notTransparent (M.getElem y' x' matrix)
                notTransparent (MyColor _ _ _ 0) = False
                notTransparent _                 = True

-- Assumes the images are the same size
imageOverlay :: MyImage -> MyImage -> MyImage
imageOverlay (MyImage bottom) (MyImage top) =
    MyImage $ M.fromList (M.nrows bottom) (M.ncols top) (map helper (zip (M.toList bottom) (M.toList top)))
    where
        helper (c,(MyColor _ _ _ 0)) = c
        helper (c,c')                = c'
                
{--------------------------------------- Individual Color Functions --------------------------------------}

colorAdd :: MyColor -> Int -> Int -> Int -> MyColor
colorAdd (MyColor r g b a) rp gp bp = mkColorBounded (r + rp) (g + gp) (b + bp) a

-- Preserves opacity
colorBlend :: MyColor -> MyColor -> Double -> MyColor
colorBlend (MyColor r1 g1 b1 a) (MyColor r2 g2 b2 _) ratio =
    (MyColor (change r1 r2)
             (change g1 g2)
             (change b1 b2)
             a)
    where
        change v1 v2 = floor ((toDouble v1) + (toDouble (v2 - v1)) * ratio)
        toDouble v = (fromIntegral (v :: Int) :: Double)


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
animationBlink :: MyColor -> Int -> MyImage -> [MyImage]
animationBlink c' frames image = map helper ([0..frames] ++ [frames-1,frames-2..0])
    where
        helper frame = imageMap (\c -> colorBlend c c' ratio) image
            where
                ratio = (toDouble frame) / (toDouble frames)
                toDouble v = (fromIntegral (v :: Int) :: Double)

-- Both color lists must be the same length
animationGlow :: [MyColor] -> [MyColor] -> Int -> MyImage -> [MyImage]
animationGlow colors colors' frames image@(MyImage matrix) = map helper ([0..frames] ++ [frames-1,frames-2..0])
    where
        rows = M.nrows matrix
        cols = M.ncols matrix
        -- Pairs up outlines with their corresponding initial colors
        outlinePairs = zip (map (\v -> imageGetOutline v image)
                                [1..(length colors)])
                           colors
        -- Uses outline-color pairs to color outlines
        coloredOutlines = map (\(image, color) -> imageReplaceAllColor color image)
                              outlinePairs
        -- Recolors based on the given frame ratio and the given ending colors
        frameOutlines ratio = map (\(image, color') -> imageMap (\c -> colorBlend c color' ratio)
                                                                image)
                                  (zip coloredOutlines colors')
        -- Places outlines on top of one another to create full outline
        helper frame = foldl imageOverlay
                             (transparentImage cols rows)
                             (reverse (frameOutlines ratio))
            where
                ratio = (toDouble frame) / (toDouble frames)
                toDouble v = (fromIntegral (v :: Int) :: Double)




                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                





