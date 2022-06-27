module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import GHC.Word

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage functionTest 1280 720)

functionTest :: Int -> Int -> PixelRGB8
functionTest x y
  -- equação do cardióide
  | ( (x - t - 50)^2 + (y - q)^2 + 2*r*(x - t) )^2 < 4*r^2*( (x - t)^2 + (y - q)^2 ) = PixelRGB8 (fromIntegral (div y 4)) (fromIntegral (div x 4)) 100
  | otherwise = PixelRGB8 (fromIntegral (div x 5)) (fromIntegral (div y 5)) 100
  where (t,q,r) = (600, 360, 150) 
-- (t,q) = centro do cardioide
-- r = raio do cardioide 


main :: IO ()
main = do
  --[path] <- getArgs
  savePngImage "teste.png" generateImg
