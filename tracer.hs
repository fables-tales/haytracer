import Data.Maybe
import Codec.Image.DevIL
import Data.Array.Unboxed
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Vector3


fileName = "fractal2345.png"

main = do
    removeIfExists fileName
    ilInit
    writeImage fileName image

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

width :: Int
width  = 244

height ::Int
height = 244

image :: UArray (Int, Int, Int) Word8
image = array ((0,0,0),(width, height,3)) render

render :: [((Int,Int,Int),Word8)]
render = do
    x <- [0..width-1]
    y <- [0..height-1]
    zip (channelIdentifiers x y) (pixel x y)

channelIdentifiers :: Int -> Int -> [(Int, Int, Int)]
channelIdentifiers x y = [(x,y,channel) | channel <- [0..3]]

pixel :: Int -> Int -> [Word8]
pixel x y = if isJust (distanceToIntersection (rayFor (Vector3 (0,0,0)) (Vector3 (0,0,1)) (Vector3 (0,1,0)) (x,y)) (Sphere (Vector3 (0,0,10)) 1))
                then [255, 0, 0, 255]
                else [0,0,0,255]

--intersection stuff

distanceToIntersection :: RayClass -> Shape -> Maybe Double
distanceToIntersection ray (Sphere center radius) = if vectorDistance (closestPoint ray center) center < radius then Just 1.0 else Nothing

closestPoint :: RayClass -> VectorClass -> VectorClass
closestPoint (Ray direction origin) point
 = addVector origin (scaled aToB t) where
     a = origin
     b = addVector origin (scaled direction 10000000000)
     aToB = b `subVector` a
     aToP = point `subVector` a
     t = dotProduct aToP aToB / len2 aToB

--end intersection stuff


--camera stuff
rayFor :: VectorClass -> VectorClass -> VectorClass -> (Int, Int) -> RayClass
rayFor position look up (x,y) = Ray (directionFor look up (x,y)) position

d      = 1.0
left   = -1.0
right  = 1.0
top    = 1.0
bottom = -1.0

directionFor :: VectorClass -> VectorClass -> (Int, Int) -> VectorClass
directionFor look up (x, y) = (normalized . addVectors) [p1, p2, p3] where
                                w  = wVector look
                                u  = uVector w up
                                v  = vVector w u
                                p1 = scaled w (-d)
                                p2 = scaled u (uForPixel x)
                                p3 = scaled v (vForPixel y)

uForPixel :: Int -> Double
uForPixel x = left + (right-left) * (fromIntegral x / fromIntegral width)

vForPixel :: Int -> Double
vForPixel y = bottom + (top-bottom) * (fromIntegral y / fromIntegral height)

wVector :: VectorClass -> VectorClass
wVector = negative . normalized

uVector :: VectorClass -> VectorClass -> VectorClass
uVector w up = (crossProduct . normalized) w (normalized up)

vVector :: VectorClass -> VectorClass -> VectorClass
vVector = crossProduct . normalized
--end camera stuff

--Ray for a camera at position, with look vector and up vector

data Shape = Sphere VectorClass Double deriving (Show, Eq)
data RayClass = Ray VectorClass VectorClass deriving (Show, Eq)
