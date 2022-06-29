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

videoPath = "meu vídeo"
-- resolução do vídeo
width = 1280
height = 720
-- duração do vídeo e fps
duracao = 10
taxa_att = 60
-- valor inicial - valor final
inicial_point = -100
final_point = 100
alteracao_total = final_point - inicial_point

generateImg :: Float -> Image PixelRGB8
generateImg w = generateImage functionTest width height
  where functionTest x y = if (( (l - t - w)^2 + (m - q)^2 + 2*r*(l - t) )^2 < 4*r^2*( (l - t)^2 + (m - q)^2 )) 
                           then PixelRGB8 (fromIntegral (div y 4)) (fromIntegral (div x 4)) 100 
                           else PixelRGB8 (fromIntegral (div x 5)) (fromIntegral (div y 5)) (fromIntegral (round w))
                        where (t,q,r,l,m) = (600, 360, 101, fromIntegral x, fromIntegral y) 
                        -- (t,q) = centro do cardioide
                        -- r = raio do cardioide 

processImg :: Float -> DynamicImage
processImg t = ImageRGB8 (generateImg t)


{--------------------------------------------------------------------------------------------
 --Applies "juicyToFFmpeg'" and "getFPS" to a list of images and saves the output-video
 --to a user defined location.
--------------------------------------------------------------------------------------------    
saveVideo :: String -> [Image PixelRGB8] -> Int -> IO ()
saveVideo path imgs fps = do
         -- program stops after hitting next line --
         frame <- frameWriter ep path
         ------------------------------------------------
         Prelude.mapM_ (frame . Just) ffmpegImgs 
         frame Nothing
         where ep = EncodingParams width height fps (Just avCodecIdMpeg4) (Just avPixFmtGray8a) "medium"
               width      = toCInt $ imageWidth  $ head imgs
               height     = toCInt $ imageHeight $ head imgs
               ffmpegImgs = juicyToFFmpeg' imgs
               toCInt x   = fromIntegral x :: CInt


--------------------------------------------------------------------------------------------
 --Converts a single image from JuicyPixel-format to ffmpeg-light-format. 
--------------------------------------------------------------------------------------------  
juicyToFFmpeg :: Image PixelRGB8 -> (AVPixelFormat, V2 CInt, Vector CUChar)
juicyToFFmpeg img = (avPixFmtGray8a, V2 (toCInt width) (toCInt height), ffmpegData)
                  where toCInt   x   = fromIntegral x :: CInt
                        toCUChar x   = fromIntegral x :: CUChar
                        width        = imageWidth img
                        height       = imageHeight img
                        ffmpegData   = VS.map toCUChar (imageData img)

--------------------------------------------------------------------------------------------
-- Converts a list of images from JuicyPixel-format to ffmpeg-light-format. 
---------------------------------------------------------------------------------------------                     
juicyToFFmpeg' :: [Image PixelRGB8] -> [(AVPixelFormat, V2 CInt, Vector CUChar)]
juicyToFFmpeg' imgs = Prelude.foldr (\i acc -> acc++[juicyToFFmpeg i]) [] imgs
---}
main :: IO ()
main = do

  let img = processImg (inicial_point)
  let intervalo = alteracao_total / duracao * taxa_att
  let value_perFrame = [inicial_point, inicial_point + intervalo .. final_point]
  let frame_list = [ generateImg x | x <- value_perFrame ]

  let teste = encodeGifAnimation 1 LoopingForever frame_list
  writeGifAnimation "meu gif" 1 LoopingForever frame_list teste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.7/docs/Codec-Picture.html#g:8