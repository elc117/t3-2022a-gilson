module Main (main) where

import Codec.Picture
import Codec.FFmpeg.Juicy ( imageWriter )
import Codec.FFmpeg
import Codec.FFmpeg.Encode
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import GHC.Word

gif_path = "Cardioide.gif"
-- resolução do vídeo
width = 1280
height = 720
-- duração do vídeo e fps
duracao = 2
taxa_att = 30
-- valor inicial - valor final
inicial_point = -100
final_point = 100
alteracao_total = final_point - inicial_point

generateImg :: Float -> Image PixelRGB8
generateImg w = generateImage function width height
  where function x y = if (( (l - t - w)^2 + (m - q)^2 + 2*r*(l - t) )^2 < 4*r^2*( (l - t)^2 + (m - q)^2 )) 
                           then PixelRGB8 (fromIntegral (div y 4)) (fromIntegral (div x 4)) 100 
                           else PixelRGB8 (fromIntegral (div x 5)) (fromIntegral (div y 5)) (fromIntegral (round w))
                        where (t,q,r,l,m) = (600, 360, 101, fromIntegral x, fromIntegral y) 
                        -- (t,q) = centro do cardioide
                        -- r = raio do cardioide 

main :: IO ()
main = do

  let intervalo = alteracao_total / (duracao * taxa_att)
  let value_perFrame = [inicial_point, inicial_point + intervalo .. final_point]
  let image_list = [ generateImg x | x <- value_perFrame ]

  case writeGifAnimation gif_path 1 LoopingForever image_list of
      Left s -> fail s
      Right i -> i

