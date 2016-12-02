import Codec.Picture.Saving
import qualified Codec.Picture as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Vector.Storable as V
import Data.Word

data MyPoint = MyPoint Int Int
data MyColor = MyColor { red     :: Int 
                       , green   :: Int 
                       , blue    :: Int
                       , opacity :: Int }

instance Show MyColor where
    show (MyColor r g b a) = "("  ++ show r ++ 
                             ", " ++ show g ++ 
                             ", " ++ show b ++ 
                             ", " ++ show a ++ ")"
                             
data MyImage = MyImage [(MyColor, MyPoint)] Int Int 

imageMap :: (MyColor -> MyColor) -> MyImage -> MyImage
imageMap f (MyImage pairs w h) = MyImage (map (\(color, point) -> ((f color), point)) pairs) w h


fromPng :: FilePath -> IO MyImage
fromPng path = do
                let
                    makeImage :: J.DynamicImage -> MyImage
                    makeImage (J.ImageRGBA8 image@(J.Image w h _)) = convertPixels image w h
                    convertPixels pixels w h = MyImage (zip (map (\(MyPoint x y) -> pixelToColor (J.pixelAt pixels x y)) points) points) w h
                                            where
                                                points = [(MyPoint x y) | x <- [0..w-1], y <- [0..h-1]]
                    pixelToColor ::  J.PixelRGBA8 -> MyColor
                    pixelToColor (J.PixelRGBA8 r g b a) = MyColor (toInt r)
                                                                  (toInt g)
                                                                  (toInt b)
                                                                  (toInt a)
                    toInt w = (fromIntegral (w :: Word8) :: Int)
                file <- B.readFile path                 -- file   :: ByteString
                let (Right result) = J.decodePng file   -- result :: DynamicImage
                return (makeImage result)   


toPng :: FilePath -> MyImage -> IO ()
toPng path image = do
                    let
                        toJImage :: MyImage -> J.Image J.PixelRGBA8
                        toJImage (MyImage pairs w h) = J.ImageRGBA8 (J.Image w h (V.fromList (map (\p -> toJPixel (fst p)) pairs))) -- Only want the pixels themselves
                        toJPixel :: MyColor -> J.PixelRGBA8
                        toJPixel (MyColor r g b a) = J.PixelRGBA8 (toWord8 r)
                                                                  (toWord8 g)
                                                                  (toWord8 b)
                                                                  (toWord8 a)
                        toWord8 i = (fromIntegral (i :: Int) :: Word8)
                    let bytestring = J.encodePng (toJImage image)
                    Bl.writeFile path bytestring


--
                        

































