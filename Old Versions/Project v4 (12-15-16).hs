{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Graphics.UI.GLUT
import Data.IORef
import Codec.Picture.Saving
import qualified Codec.Picture as J
import Codec.Picture.ColorQuant
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Vector.Storable as V
import Data.Word
import qualified Data.Matrix as M


{----------------------------------- DEMO section ---------------------------------------}

exF10 = imageGetOutline 1
exF11 = imageReplaceColor 10 (MyColor 255 255 255 0) (MyColor 255 0 0 1)
exF12 = imageReplaceColor 10 (MyColor 255 255 255 0) (MyColor 0 255 0 1)
exF13 = imageExpandBorders exColorTransparent 2 2 2 2

test10 = exImg img1 (exF11 . exF10 . exF13)

test11 = exImg img1 (\i -> (imageOverlay (imageExpandAndOutline 1 i)) (imageExpandBorders exColorTransparent 1 1 1 1 i))
test12 = exAni (fromPng "Sprites/MarioBig.png") ((\i -> animationLengthen 200 i) .
                                                 (\i -> splitSpriteSheet i 4 1))
test13 = exAni (fromPng "Sprites/KabutopsSingle.png") (\i -> animationRainbow 50 0.3 i)


test1 = exAni img1 (animationBlink (MyColor 255 0 0 1) 300)
test2 = exBoth img2 (exF4 . exF3 . exF1) (exA2 . exF4)
test4 = exAniTwo img1 (exA3 . exF4) (exA1 .
                                     (\i -> imageReplaceAllColor (MyColor 0 0 0 1) i) .
                                     (\i -> imageGetOutline 1 i) .
                                     exF4)
test5 = exAniTwo img5 (exA3 . exF4) (exA2 . exF4)                         
test6 = exAniTwo img2 (exA2 . exF4) ((\a -> animationMap imageInvert a) .
                                     exA3 .
                                     exF4)
 



colors1 = [(MyColor 200 200 255 1), (MyColor 50  50  150 1)]
colors2 = [(MyColor 100 100 255 1), (MyColor 255 255 255 1)]
                                
                                
exF1 = imageReplaceColor 80 (MyColor 184 64 64 1) (MyColor 0 255 0 1)
exF2 = imageMap (\c -> colorAdd (-20) 20 0 c)
exF3 = imageInvert

-- Expand image 5 pixels on all sides with transparent pixels
exF4 = imageExpandBorders exColorTransparent 5 5 5 5
exF5 = imageGetOutline 1

exF6 = imageGrayscale

exF7 = imageReplaceAllColor (MyColor 255 0 255 1)

exA1 = animationBlink (MyColor 255 255 255 1) 120
exA2 = animationGlow colors1 colors2 300
exA3 = animationRainbow 500 0.5 
 
imageExpandAndOutline v i = (imageGetOutline v (imageExpandBorders exColorTransparent v v v v i))
 
 
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
                    pixelToColor :: J.PixelRGBA8 -> MyColor
                    pixelToColor (J.PixelRGBA8 r g b a) = MyColor (toInt r)
                                                                  (toInt g)
                                                                  (toInt b)
                                                                  (alphaConvert a)
                    toInt w = (fromIntegral (w :: Word8) :: Int)
                    alphaConvert w = (fromIntegral (w :: Word8) :: Double) / 255

                file <- B.readFile path                 -- file   :: ByteString
                let (Right result) = J.decodePng file   -- result :: DynamicImage
                return (makeImage result)

toJImage :: MyImage -> J.Image J.PixelRGBA8
toJImage (MyImage matrix) = J.generateImage genPixel w h
    where
        w = M.ncols matrix
        h = M.nrows matrix
        toWord8 v = (fromIntegral (v :: Int) :: Word8)
        genPixel x y = J.PixelRGBA8 (toWord8 r)
                                    (toWord8 g)
                                    (toWord8 b)
                                    (ceiling (a * 255))
            where (MyColor r g b a) = M.getElem (1 + y) (1 + x) matrix

                
toPng :: FilePath -> MyImage -> IO ()
toPng path image = do
                    Bl.writeFile path (J.encodePng (toJImage image))
     
ioToPng :: FilePath -> IO MyImage -> (MyImage -> MyImage) -> IO ()
ioToPng path ioImg f = do
                        image <- ioImg
                        toPng path (f image)
                        
-- Delay is in 1/100th of second
toGif :: FilePath -> [MyImage] -> Int -> IO ()
toGif path images delay = do
                            let (Right result) = J.encodeGifImages J.LoopingForever 
                                                                   [(palette, delay, image) |
                                                                        (image, palette) <- J.palettize defaultPaletteOptions <$> jImages]
                            Bl.writeFile path result
    where
        jImages = map (\i -> J.pixelMap dropTransparency (toJImage (imageSetTransparentWhite i))) images        
  
{---------------------------------------------------------------------------------}

exColorTransparent = (MyColor 255 255 255 0)
exColorWhite       = (MyColor 255 255 255 1)

{--------------------------- Loading some examples -------------------------------}

img1   = fromPng "Sprites/MarioSmall_1.png"
img2   = fromPng "Sprites/MarioBig_1.png"
img3   = fromPng "Sprites/Test/white1616.png"
img4   = fromPng "Sprites/Test/red1616.png"
img5   = fromPng "Sprites/Megaman.png"

backgroundRaw = fromPng "Sprites/BackgroundSmall.png"

exImg :: IO MyImage -> (MyImage -> MyImage) -> IO ()
exImg raw f = do
                img <- raw
                let image = toDrawable (f img) 0 0 1.0 1.0
                makeWindow (return image)
                           (return exBlankAnimation)   
                           (return exBlankAnimation)

exAni :: IO MyImage -> (MyImage -> [MyImage]) -> IO ()
exAni raw f = do
                img <- raw
                let animation = mkAnimation (f img) 0 0 1.0 1.0
                makeWindow (return exBlankImage)
                           (return animation)
                           (return exBlankAnimation)
                
exBoth :: IO MyImage -> (MyImage -> MyImage) -> (MyImage -> [MyImage]) -> IO ()
exBoth raw fi fa = do
                    img <- raw
                    let image     = toDrawable  (fi img) 0 0 1.0 1.0
                    let animation = mkAnimation (fa img) 0 0 1.0 1.0
                    makeWindow (return image)
                               (return animation)
                               (return exBlankAnimation)
                    
exAniTwo :: IO MyImage -> (MyImage -> [MyImage]) -> (MyImage -> [MyImage]) -> IO ()
exAniTwo raw fa1 fa2 = do
                        img <- raw
                        let animation1 = mkAnimation (fa1 img) 0 0 1.0 1.0
                        let animation2 = mkAnimation (fa2 img) 0 0 1.0 1.0 
                        makeWindow (return exBlankImage)
                                   (return animation1)
                                   (return animation2)

                       
exBlankImage = toDrawable (transparentImage 1 1) 0 0 0.0 0.0
exBlankAnimation = mkAnimation [(transparentImage 1 1)] 0 0 0.0 0.0

example1 = do 
            img <- img1
            return $ toDrawable (exF5 (exF4 (exF3 (exF2 (exF1 img)))))
                                5 5 0.25 0.30
                                    
example2 = do
            img <- img2
            return $ mkAnimation (exA1 (exF4 img))
                                 25 5 0.6 0.4
                      
example3 = do
            img <- img2
            return $ mkAnimation (exA2 (exF4 img))
                                 25 5 0.6 0.4
                                     
example4 = do
            img <- img2
            let (MyImage matrix) = img
            return $ toDrawable (imageOverlay (blankImage (M.ncols matrix) (M.nrows matrix)) img)
                                10 10 0.3 0.3        
background = do img <- backgroundRaw 
                return $ toDrawable img 0 0 1.0 1.0

{----------------------------------  Main and Display ----------------------------}

-- Main Function
-- Yeah I have no idea what any of this does

{-
main :: IO ()
main i a = do
              (_progName, _args) <- getArgsAndInitialize
              initialWindowSize $= Size 640 480
              _window <- createWindow "Test"
              blend $= Enabled
              blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
              viewport $= (Position 0 0, Size (fromIntegral 100) (fromIntegral 100))
              iter <- newIORef 1  
              displayCallback $= display i a iter
              reshapeCallback $= Just reshape
              idleCallback $= Just (idle iter)  
              mainLoop
-}
              
makeWindow :: IO Drawable -> IO Animation -> IO Animation -> IO ()
makeWindow i a1 a2 = do
              (_progName, _args) <- getArgsAndInitialize
              initialWindowPosition $= Position 0 0
              initialWindowSize $= Size 640 900
              _window <- createWindow "Test"
              blend $= Enabled
              blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
              viewport $= (Position 0 0, Size (fromIntegral 100) (fromIntegral 100))
              iter <- newIORef 1  
              displayCallback $= display i a1 a2 iter
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

display :: IO Drawable -> IO Animation -> IO Animation -> IORef Int -> DisplayCallback
display img ani1 ani2 iter = do
  clear [ ColorBuffer ]       -- clears canvas?
  iteration  <- get iter
  image      <- img
  animation1 <- ani1
  animation2 <- ani2
  
  draw image
  drawAnimation animation1 iteration
  drawAnimation animation2 iteration
  
  {-
  i1 <- background
  i2 <- example1
  i3 <- example4

  a1 <- example2
  a2 <- example3
  
  --drawCanvas
  --draw i1
  draw i2
  --draw i3
  drawAnimation a1 iteration
  drawAnimation a2 iteration
  -}
  flush                       -- Sends openGL commands to graphics for display

draw :: Drawable -> IO ()
draw (Drawable triplet w h) = do
                                mapM_ (\(pixelColor, v1, v2) -> do
                                                                    color pixelColor
                                                                    rect v1 v2)
                                      triplet
                                      
drawAnimation :: Animation -> Int -> IO ()
drawAnimation animation frame = draw (getFrame animation frame)
                 
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
                rotate b v = (b + (v * 2)) `mod` 255 
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
                changePixel (_, point@(MyPoint x y)) = ((MyColor (round (((toFloat x) / (toFloat w)) * 255))
                                                                 (round (((toFloat y) / (toFloat h)) * 255))
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
                          r < 255 && g < 255 && b < 255 &&
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

{------------------------------ Animation --------------------------------------------------}
        
-- Maintains speed of animation as number of loops per frame
data Animation = Animation [Drawable] Int

getFrame :: Animation -> Int -> Drawable
getFrame (Animation imgs _) frame = (!!) imgs (frame `mod` (length imgs))

animationMap :: (MyImage -> MyImage) -> [MyImage] -> [MyImage]
animationMap f images = map f images

mkAnimation :: [MyImage] -> Int -> Int -> Double -> Double -> Animation
mkAnimation images x y w h = Animation (map (\i -> (toDrawable i x y w h)) images) (length images)

-- Divides the sheet into n images
-- b is the number of pixels between each sprite
splitSpriteSheet :: MyImage -> Int -> Int -> [MyImage]
splitSpriteSheet (MyImage matrix) n b = map helper (take n [1,(imgWidth + 2)..])
    where
        imgWidth = (width - ((n - 1) * b)) `div` n
        helper v = MyImage $ M.submatrix 1 height v (v + imgWidth - 1) matrix
        width = M.ncols matrix
        height = M.nrows matrix

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
        scaleColor c = (toDouble c) / 255
        helper (MyColor r g b a) px py = (newColor, Vertex2 x1 y1, Vertex2 x2 y2)
            where
                -- Not actually magic numbers
                x1 = (((toDouble (px + x - 1)) / (toDouble cols) * 2 * w) - 1)
                x2 = x1 + pixelWidth
                -- For some reason the juicypixels people made it so the top left
                -- is the origin instead of the standard of having the bottom left
                -- It could be backwards I'm too lazy to check.
                y1 = (((toDouble (rows - py + y + 1)) / (toDouble rows) * 2 * h) - 1)
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

imageTranspose :: MyImage -> MyImage
imageTranspose (MyImage matrix) = MyImage (M.transpose matrix)

imageReplaceColor :: Int -> MyColor -> MyColor -> MyImage -> MyImage
imageReplaceColor m compare@(MyColor r' g' b' _) new image = imageMap helper image
    where
        helper old@(MyColor _ _ _ 0)          = old
        helper old@(MyColor r g b _)
            | (colorRangeRed   m old compare) && 
              (colorRangeGreen m old compare) &&
              (colorRangeBlue  m old compare) = new
            | otherwise                       = old 
                
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

                
imageExpandBorders :: MyColor -> Int -> Int -> Int -> Int -> MyImage -> MyImage
imageExpandBorders replace left right up down (MyImage old) = MyImage new
    where
        rows = M.nrows old
        cols = M.ncols old
        s1 = M.extendTo replace           -- Extend right and bottom border
                        (down  + rows)
                        (right + cols)
                        old
        s2 = flip s1                      -- Reverse rows, transpose, reverse cols
        s3 = M.extendTo replace
                        (up   + rows + down)
                        (left + cols + right)
                        s2
        new = flip s3 
        rev = map reverse . M.toLists
        tra = M.transpose . M.fromLists
        flip = tra . rev . tra . rev

        
imgStretchUD :: Int -> MyImage -> MyImage
imgStretchUD times i = imageTranspose (imgStretchLR times (imageTranspose i))

imgStretchLR :: Int -> MyImage -> MyImage
imgStretchLR times image = MyImage (M.fromLists (map concat
                                                (M.toLists (matrixMap (\c -> f c)
                                                                      (getMatrix (expand image))))))
    where
        expand i = imageExpandBorders exColorTransparent 0 (imageGetWidth i) 0 0 i
        f c = replicate times c
        getMatrix (MyImage matrix) = matrix
                
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
        
imageConditionMap :: (MyColor -> Bool) -> (MyColor -> MyColor) -> MyImage -> MyImage
imageConditionMap cond f image = imageMap (\c -> colorWithCondition f cond c) image
                
conditionTest = exImg img2 (\i -> imageConditionMap (\b -> not (colorIsOpaque b)) (\c -> colorAdd 100 100 100 c) i)
                
-- For now assumes same dimensions
imageBlend :: Double -> MyImage -> MyImage -> MyImage
imageBlend ratio img1@(MyImage m1) img2@(MyImage m2) =
    MyImage $ M.fromList (imageGetHeight img1)
                         (imageGetWidth  img1)
                         (map (\(a,b) -> colorBlend ratio a b)
                              (zip (M.toList m1)
                                   (M.toList m2)))
                                   
imageSetTransparentWhite :: MyImage -> MyImage
imageSetTransparentWhite image = imageMap helper image
    where
        helper (MyColor _ _ _ 0) = (MyColor 255 255 255 0)
        helper c = c

        
{--------------------------------------- Image Utility Functions -----------------------------------------}

imageGetHeight :: MyImage -> Int
imageGetHeight (MyImage matrix) = M.nrows matrix

imageGetWidth  :: MyImage -> Int
imageGetWidth  (MyImage matrix) = M.ncols matrix




                
                
                
{--------------------------------------- Individual Color Functions --------------------------------------}

colorBlend :: Double -> MyColor -> MyColor -> MyColor
colorBlend _ (MyColor _ _ _ 0) c = c
colorBlend _ c (MyColor _ _ _ 0) = c
colorBlend ratio (MyColor r1 g1 b1 a1) (MyColor r2 g2 b2 a2) = (MyColor (f1 ratio r1 r2)
                                                                        (f1 ratio g1 g2)
                                                                        (f1 ratio b1 b2)
                                                                        (f2 ratio a1 a2))
    where
        f1 ratio v1 v2 = round ((toDouble v1) * ratio + (toDouble v2) * (1 - ratio))
        f2 ratio v1 v2 = v1 * ratio + v2 * (1 - ratio)
        toDouble v = (fromIntegral (v :: Int) :: Double)

colorWithCondition :: (MyColor -> MyColor) -> (MyColor -> Bool) -> MyColor -> MyColor
colorWithCondition f cond old
    | cond old  = f old
    | otherwise = old

colorAdd :: Int -> Int -> Int -> MyColor -> MyColor
colorAdd rp gp bp (MyColor r g b a) = mkColorBounded (r + rp) (g + gp) (b + bp) a

-- Preserves opacity
colorChangeTo :: MyColor -> MyColor -> Double -> MyColor
colorChangeTo old@(MyColor _ _ _ 0) _ _ = old
colorChangeTo (MyColor r1 g1 b1 a1) (MyColor r2 g2 b2 a2) ratio =
    (MyColor (change r1 r2)
             (change g1 g2)
             (change b1 b2)
             (a1 + ((a2 - a1) * ratio)))
    where
        change v1 v2 = floor ((toDouble v1) + (toDouble (v2 - v1)) * ratio)
        toDouble v = (fromIntegral (v :: Int) :: Double)


        
{----------------------------------------- Color Utility Functions ---------------------------------------}

colorRangeRed :: Int -> MyColor -> MyColor -> Bool
colorRangeRed   range (MyColor r _ _ _) (MyColor r' _ _ _) = inRange r r' range

colorRangeGreen :: Int -> MyColor -> MyColor -> Bool
colorRangeGreen range (MyColor _ g _ _) (MyColor _ g' _ _) = inRange g g' range

colorRangeBlue :: Int -> MyColor -> MyColor -> Bool
colorRangeBlue  range (MyColor _ _ b _) (MyColor _ _ b' _) = inRange b b' range

colorRangeAlpha :: Double -> MyColor -> MyColor -> Bool
colorRangeAlpha range (MyColor _ _ _ a) (MyColor _ _ _ a') = (abs (a' - a)) <= range

inRange :: Int -> Int -> Int -> Bool
inRange v v' range = (abs (v' - v)) <= range

colorIsOpaque :: MyColor -> Bool
colorIsOpaque (MyColor _ _ _ 0) = False
colorIsOpaque _                 = True



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
        helper frame = imageMap (\c -> colorChangeTo c c' ratio) image
            where
                ratio = (toDouble frame) / (toDouble frames)
                toDouble v = (fromIntegral (v :: Int) :: Double)
                
-- Rather than generate a blink off of a single animation,
-- we apply it to an animation
animationMapBlink :: MyColor -> [MyImage] -> [MyImage]
animationMapBlink c' images = map helper (zip images frameList)
    where
        helper (image,frame) = imageMap (\c -> colorChangeTo c c' ratio) image
            where
                ratio = (toDouble frame) / (toDouble numImages)
                toDouble v = (fromIntegral (v :: Int) :: Double)                
        frameList
            | numImages `mod` 2 == 0 = [0..half] ++ [half-1,half-2..1] -- Even
            | otherwise              = [0..half] ++ [half,half-1..1]
        numImages = length images
        half = numImages `div` 2

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
        frameOutlines ratio = map (\(image, color') -> imageMap (\c -> colorChangeTo c color' ratio) image)
                                  (zip coloredOutlines colors')
        -- Places outlines on top of one another to create full outline
        helper frame = foldl imageOverlay
                             (transparentImage cols rows)
                             (reverse (frameOutlines ratio))
            where
                ratio = (toDouble frame) / (toDouble frames)
                toDouble v = (fromIntegral (v :: Int) :: Double)

animationRainbow :: Int -> Double ->  MyImage -> [MyImage]
animationRainbow frames ratio img@(MyImage matrix) = map colorImageFramePair pairs
    where
        pairs :: [([([MyColor], Int)],Int)]
        pairs = zip (replicate frames (zip (M.toLists matrix) [0..(M.nrows matrix)])) [0..frames]
        
        colorImageFramePair :: ([([MyColor], Int)],Int) -> MyImage
        colorImageFramePair (rowPairs,frame) = imageBlend ratio (MyImage (M.fromLists (colorAllRows frame rowPairs))) img
        
        colorAllRows :: Int -> [([MyColor], Int)] ->  [[MyColor]]
        colorAllRows frame rowPairs = map (\(row,rowNum) -> colorRow row frame rowNum) rowPairs

        colorRow :: [MyColor] -> Int -> Int -> [MyColor]
        colorRow row frame rowNum = map (\c -> colorPixel c frame rowNum) row
                
        colorPixel :: MyColor -> Int -> Int -> MyColor
        colorPixel c frame rowNum = colorWithCondition (\clr -> colorToRainbow clr
                                                                              (frame + rowNum))
                                                       colorIsOpaque
                                                       c
                                                       
        colorToRainbow :: MyColor -> Int -> MyColor
        colorToRainbow (MyColor r g b a) frameRowMod = MyColor (toRainbowColor r frameRowMod 0)
                                                               (toRainbowColor g frameRowMod gmod)
                                                               (toRainbowColor b frameRowMod bmod)
                                                               a
            where
                gmod = (2 * pi / 3)
                bmod = (4 * pi / 3)
                toRainbowColor c frameRowMod v = round ((sin ((2 * pi / (toDouble frames) * (toDouble frameRowMod)) + v)) * 127 + 128) 
                toDouble v = (fromIntegral (v :: Int) :: Double)
                
animationRainbowMap :: Double -> [MyImage] -> [MyImage]
animationRainbowMap ratio images = map colorImageFramePair pairs
    where
        frames = length images
        
        joinedRows :: [[([MyColor], Int)]]
        joinedRows = map (\(MyImage matrix) -> (zip (M.toLists matrix) [0..(M.nrows matrix)])) images

        pairs :: [([([MyColor], Int)],Int)]
        pairs = zip joinedRows [0..frames]
        
        colorImageFramePair :: ([([MyColor], Int)],Int) -> MyImage
        colorImageFramePair (rowPairs,frame) = imageBlend ratio (MyImage (M.fromLists (colorAllRows frame rowPairs))) 
                                                                (MyImage (M.fromLists (map (\(f,_) -> f) rowPairs)))
        
        colorAllRows :: Int -> [([MyColor], Int)] ->  [[MyColor]]
        colorAllRows frame rowPairs = map (\(row,rowNum) -> colorRow row frame rowNum) rowPairs

        colorRow :: [MyColor] -> Int -> Int -> [MyColor]
        colorRow row frame rowNum = map (\c -> colorPixel c frame rowNum) row
                
        colorPixel :: MyColor -> Int -> Int -> MyColor
        colorPixel c frame rowNum = colorWithCondition (\clr -> colorToRainbow clr
                                                                              (frame + rowNum))
                                                       colorIsOpaque
                                                       c
                                                       
        colorToRainbow :: MyColor -> Int -> MyColor
        colorToRainbow (MyColor r g b a) frameRowMod = MyColor (toRainbowColor r frameRowMod 0)
                                                               (toRainbowColor g frameRowMod gmod)
                                                               (toRainbowColor b frameRowMod bmod)
                                                               a
            where
                gmod = (2 * pi / 3)
                bmod = (4 * pi / 3)
                toRainbowColor c frameRowMod v = round ((sin ((2 * pi / (toDouble frames) * (toDouble frameRowMod)) + v)) * 127 + 128) 
                toDouble v = (fromIntegral (v :: Int) :: Double)

               
{--------------------------------------- Utility Animation Functions ---------------------------------------------}

listLengthen :: Int -> [a] -> [a]
listLengthen times images = foldl (++) [] (map (\i -> replicate times i) images)
                
-- Conveninece function with type condition
animationLengthen :: Int -> [MyImage] -> [MyImage]
animationLengthen = listLengthen
                
animationRepeat :: Int -> [MyImage] -> [MyImage]
animationRepeat times imgs = foldl (++) [] (replicate times imgs)
                
                
{------------------------------------------------------- GENERATED IMAGES ---------------------------------------------}

generateAllSprites :: IO ()
generateAllSprites = do
                    demoBigMarioPurpleOutline
                    demoGreenSmallMario
                    demoKabutopsGifBlink
                    demoKabutopsGifRainbow

-- Generates a green mario
demoBigMarioPurpleOutline =
    ioToPng "Generated/Images/BigMarioPurpleOutline.png" (fromPng "Sprites/MarioBig_1.png")
                                                         ((\i -> imageReplaceAllColor (MyColor 255 0 255 1) i) .
                                                         (\i -> imageGetOutline 1 i) .
                                                         (\i -> imageExpandBorders exColorTransparent 1 1 1 1 i))                
                
demoGreenSmallMario =
    ioToPng "Generated/Images/GreenSmallMario.png" (fromPng "Sprites/MarioSmall_1.png")
                                                   (\i -> imageReplaceColor 80 (MyColor 180 64 64 1) (MyColor 0 255 0 1) i)
       
demoKabutopsGifBlink = do
                            img <- fromPng "Sprites/KabutopsSheet.png"
                            toGif "Generated/Gifs/KabutopsBlink.gif" (animationMapBlink (MyColor 255 0 0 1) (splitSpriteSheet img 47 1)) 5

demoKabutopsGifRainbow = do     
                            img <- fromPng "Sprites/KabutopsSheet.png"
                            toGif "Generated/Gifs/KabutopsRainbow.gif" (animationMapBlink (MyColor 255 255 255 1) (animationRainbowMap 0.3 (splitSpriteSheet img 47 1))) 5

              





