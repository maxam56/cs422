{-# LANGUAGE FlexibleContexts #-}
import Data.Image.Boxed hiding (label, dinstanceTransform)
import Data.Image.Interactive
import Data.Image.Internal 
import Data.Image.Binary
import Data.Image.IO
import GHC.Float

idealLowpass :: Int -> Float -> GrayImage
idealLowpass d width = makeImage d d (\row col -> let dim = fromIntegral d
                                                      r = fromIntegral $ if row < imgCent then row else dim-row
                                                      c = fromIntegral $ if col < imgCent then col else dim-col in if sqrt(r*r+c*c) < width then 1 else 0)
  where imgCent = div (d-1) 2
                       
idealBandpass :: Int -> Float -> Float -> GrayImage
idealBandpass d cent width = img2 - img1 where img2 = idealLowpass d cent
                                               img1 = idealLowpass d width

gVal :: Double -> Double -> Float -> Double                                                                     
gVal x y var = (1/(sqrt (2*pi*v)))*exp (-(((x*x)+(y*y))/(2*v)))
  where v = float2Double var

gaussianLowpass :: Int -> Float -> GrayImage
gaussianLowpass d var = makeImage d d (\row col -> let dim = fromIntegral d
                                                       r = fromIntegral $ if row < imgCent then row else dim-row
                                                       c = fromIntegral $ if col < imgCent then col else dim-col in gVal c r var)
  where imgCent = div (d-1) 2

gaussianBandpass :: Int -> Float -> Float -> GrayImage
gaussianBandpass d center var = makeImage d d (\row col -> let dim = fromIntegral d
                                                               r = fromIntegral $ if row < imgCent then row else dim-row
                                                               c = fromIntegral $ if col < imgCent then col else dim-col
                                                               rad = sqrt(r*r+c*c) in gVal (abs $ rad-cent) (abs $ rad-cent) var)
  where imgCent = div (d-1) 2
        cent = float2Double center
                                                       
main = do
  house <- readImage "im1i8.pgm"
  display house
  
  let filter = idealLowpass 256 16
  display filter
  let filterHouse = realPart . ifft $ (fft house)*(fft filter)
  display filterHouse

  let filter = gaussianLowpass 256 32
  display filter
  let filterHouse = realPart . ifft $ (fft house)*(fft filter)
  display filterHouse
  
  let filter = idealBandpass 256 32 16
  display filter
  let filterHouse = realPart . ifft $ (fft house)*(fft filter)
  display filterHouse

  let filter = gaussianBandpass 256 32 16
  display filter
  let filterHouse = realPart . ifft $ (fft house)*(fft filter)
  display filterHouse
  
