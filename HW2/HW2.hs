{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
--Background image source: https://www.photoshopcreative.co.uk/image/29031/alien_planet
import Data.Image.Boxed
import Data.Image.Interactive
import Data.Image.Internal 

centerImage :: Int -> Int -> ColorImage -> ColorImage
centerImage row col img = makeImage row col (\r c -> if (r > rOrig && c > cOrig) && (r <= rMax && c <= cMax)
                                              then ref img (r-rOrig-1) (c-cOrig-1)
                                              else RGB (1,200,1))
--                                              else (RGB (1,1,1))
  where (rDim, cDim) = dimensions img
--        rOrig = floor $ ((fromIntegral row) / 2) - ((fromIntegral rDim) / 2)
        rOrig = floor $ (fromIntegral (row - rDim)) / 2
--        cOrig = floor $ ((fromIntegral col) / 2) - ((fromIntegral rDim) / 2)
        cOrig = floor $ (fromIntegral (col - cDim)) / 2
        rMax = rOrig + rDim
        cMax = cOrig + cDim

--makeBinary :: (Image img, BinaryPixel (Pixel img)) => ColorImage -> [(Int, Int)] -> img
makeBinary img [rRange,gRange,bRange] = toBinaryImage (\(RGB (r,g,b)) -> (r > (fromIntegral rMin) && r < (fromIntegral rMax))
                              && (g > (fromIntegral gMin) && g < (fromIntegral gMax))
                              && (b > (fromIntegral bMin) && b < (fromIntegral bMax))) img
    where (rMin,rMax) = (fst rRange,snd rRange)
          (gMin,gMax) = (fst gRange,snd gRange)
          (bMin,bMax) = (fst bRange,snd bRange)

makeComposite :: ColorImage -> ColorImage -> [(Int,Int)] -> ColorImage
makeComposite img grn ranges@[rRange,bRange,gRange] = makeImage row col (\r c -> if (ref bin r c) == 0 then (ref grn r c) else (ref img r c))
--  where (rMin,rMax) = (fst rRange,snd rRange)
--       (gMin,gMax) = (fst gRange,snd gRange)
--        (bMin,bMax) = (fst bRange,snd bRange)
  where bin = makeBinary grn ranges
        (row,col) = dimensions img

makeCompositeGreen :: ColorImage -> ColorImage -> (Int,Int) -> ColorImage
makeCompositeGreen img grn (min,max) = makeImage row col (\r c -> if (ref bin r c) == 1 then (ref grn r c) else (ref img r c)) 
  where bin = toBinaryImage (\(RGB (r,g,b)) -> r > 0 && (g > (fromIntegral min) && g < (fromIntegral max)) && b > 0) grn
        (row,col) = dimensions img

makeCompositeBlue :: ColorImage -> ColorImage -> (Int,Int) -> ColorImage
makeCompositeBlue img blu (min,max) = makeImage row col (\r c -> if (ref bin r c) == 1 then (ref blu r c) else (ref img r c)) 
  where bin = toBinaryImage (\(RGB (r,g,b)) -> r > 0 && (b < (fromIntegral min) || b > (fromIntegral max)) && g > 0) blu
        (row,col) = dimensions img


main = do
  planet <- readColorImage "planet.ppm"
  pred <- readColorImage "pred.ppm"
  let pred_crop = crop 0 0 305 500 pred
  let pred_cent = centerImage 961 1024 pred_crop
  putStrLn $ show (dimensions pred)
  putStrLn $ show (dimensions planet)
  let composite = makeComposite planet pred_cent [(0,255), (150,210), (0,255)]
  display composite

