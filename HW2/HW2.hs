{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
--Background image source: https://www.photoshopcreative.co.uk/image/29031/alien_planet
import Data.Image.Boxed hiding (label, dinstanceTransform)
import Data.Image.Interactive
import Data.Image.Internal 
import Data.Image.Binary
import Data.Vector.Unboxed hiding (map)
import Data.Array
import Data.Text.Array

--greeness :: ColorImage -> Double ->  GrayImage
greeness img thresh = makeImage row col (\r c -> let (RGB (rp,gp,bp)) = (ref img r c)
                                                     grn = (gp / (rp+gp+bp)) in if grn > thresh then 0 else 255)
  where (row, col) = dimensions img

--greeness :: ColorImage -> Double -> GrayImage
--greeness img thresh = fmap (\(RGB (r,g,b)) -> let grn = g / (r+b+g) in if grn > thresh then 0 else 255) img

--greeness img = ((colorImageGreen img) / ((colorImageGreen img) + (colorImageRed img) + (colorImageBlue img)))

--makeDisk :: Int -> GrayImage
--makeDisk dim = makeImage dim dim (\r c ->
--                                 let rad = fromIntegral $ dim `div` 2
--                                     row = fromIntegral $ (r-rad) * (r-rad)
--                                    col = fromIntegral $ (c-rad) * (c-rad)
--                                     l = sqrt ((row + col) :: Double) in if l < (fromIntegral rad) then 1 else 0)

makeDisk dim = [[v | c<-[0..dim], let rad = fromIntegral $ dim `div` 2,
                                  let row = fromIntegral $ (r-rad)*(r-rad),
                                  let col = fromIntegral $ (c-rad)*(c-rad),
                                  let l = sqrt ((row+col)::Double),
                                  let v = if l < (fromIntegral rad) then 1 else 0 ] | r<-[0..dim]]
centerImage :: Int -> Int -> ColorImage -> ColorImage
centerImage row col img = makeImage row col (\r c -> if (r > rOrig && c > cOrig)
                                              && (r <= rMax && c <= cMax)
                                              then ref img (r-rOrig-1) (c-cOrig-1)
                                              else RGB (1,200,1))
  where (rDim, cDim) = dimensions img
        rOrig = floor $ (fromIntegral (row - rDim)) / 2
        cOrig = floor $ (fromIntegral (col - cDim)) / 2
        rMax = rOrig + rDim
        cMax = cOrig + cDim

makeBinary img [rRange,gRange,bRange] = toBinaryImage (\(RGB (r,g,b)) -> (r > (fromIntegral rMin) && r < (fromIntegral rMax))
                              && (g > (fromIntegral gMin) && g < (fromIntegral gMax))
                              && (b > (fromIntegral bMin) && b < (fromIntegral bMax))) img
    where (rMin,rMax) = (fst rRange,snd rRange)
          (gMin,gMax) = (fst gRange,snd gRange)
          (bMin,bMax) = (fst bRange,snd bRange)

makeComposite :: ColorImage -> ColorImage -> [(Int,Int)] -> ColorImage
makeComposite img grn ranges@[rRange,bRange,gRange] = makeImage row col (\r c -> if (ref bin r c) == 0 then (ref grn r c) else (ref img r c))
  where bin = close' $ makeBinary grn ranges
        (row,col) = dimensions img

makeCompositeGreen :: ColorImage -> ColorImage -> (Int,Int) -> ColorImage
makeCompositeGreen img grn (min,max) = makeImage row col (\r c -> if (ref bin r c) == 1 then (ref grn r c) else (ref img r c)) 
  where bin = toBinaryImage (\(RGB (r,g,b)) -> r > 0 && (g > (fromIntegral min) && g < (fromIntegral max)) && b > 0) grn
        (row,col) = dimensions img

makeCompositeBlue :: ColorImage -> ColorImage -> (Int,Int) -> ColorImage
makeCompositeBlue img blu (min,max) = makeImage row col (\r c -> if (ref bin r c) == 1 then (ref blu r c) else (ref img r c)) 
  where bin = toBinaryImage (\(RGB (r,g,b)) -> r > 0 && (b < (fromIntegral min) || b > (fromIntegral max)) && g > 0) blu
        (row,col) = dimensions img

thresh_dt img thresh = dt .> thresh where dt = Data.Image.Binary.distanceTransform img

process_coins :: ColorImage -> GrayImage
process_coins img = (open (makeDisk 5) (grn_bin .== 0)) .== 0
  where grn = greeness img 0.7 :: GrayImage
        grn_bin = toBinaryImage (\x->x>0) grn

numType :: (Double -> Bool) -> [Double] -> Int
numType pred xs = Prelude.sum (Prelude.map (\x -> if pred x then 1 else 0) xs)
dimes x = (x > 1000) && (x < 1500)
pennies x = (x > 1500) && (x < 2100)
nickels x = (x > 2100) && (x < 3000)
quarters x = (x > 3000) && (x < 7000)

coins = [quarters, nickels, pennies, dimes]

flipBin bin = makeImage row col (\r c -> if (ref bin r c) == 0 then 1 else 0) :: ColorImage
  where (row,col) = dimensions bin

getAreas bin = Data.Vector.Unboxed.toList $ areas labels where labels = label bin :: GrayImage

coinValue img =  (fromIntegral $ 10*(numType dimes area) + 1*(numType pennies area) + 5*(numType nickels area) + 25*(numType quarters area)) / 100
  where coin_proc = process_coins img
        dt_t = thresh_dt coin_proc 70 :: GrayImage
        labels = label dt_t :: GrayImage
        area = Data.Vector.Unboxed.toList $ areas labels

--paint_comp :: GrayImage -> Int -> GrayImage
--paint_comp img low high = imageMap (\x -> let a = (img_a Data.Vector.Unboxed.! (floor x)) in
--                                       if a >= (fromIntegral low) && a <= (fromIntegral high) then 1 else 0) img_l 
--  where img_l = label img :: GrayImage
--        img_a = areas img_l

remove_small img thresh = imageMap (\x -> let a = (img_a Data.Vector.Unboxed.! (floor x)) in
                                       if a > (fromIntegral thresh) then 1 else 0) img_l
  where img_l = label img :: GrayImage
        img_a = areas img_l

paint_comp img thresh = imageMap (\x -> let a = (img_a Data.Vector.Unboxed.! (floor x)) in
                                       if a < (fromIntegral thresh) then 1 else 0) img_l 
  where img_l = label img :: GrayImage
        img_a = areas img_l

fill_holes img thresh = paint_comp img_i thresh
  where img_i = img .== 0

make_struct r c = Prelude.map (\_ -> Prelude.map (\_ -> 1) [1..c]) (Prelude.map (\_ -> 1) [1..r])

color_outline img r g b = fmap (\x -> if x == 1 then (RGB (r,g,b)) else 0) (outline img)

square_pads img = open (make_struct 12 12) img

remove_shape img mask = open (make_struct 2 2) $ fmap (\x->if x /= 1 then 0 else 1) subImg :: GrayImage
  where subImg = img - mask

main = do
  coins1 <- readColorImage "coins/coins1.ppm"
 -- display coins1
  --plotHistograms $ colorImageToRGB coins1
  --let bin = makeBinary coins1 [(0,255), (0,130), (0,255)]
  --let bin = toBinaryImage (\(RGB (r,g,b)) -> r < 130 && g > 130 && b < 130) coins1
  --display bin
  --let bin_e = erode' bin
  --let (row,col) = dimensions bin
  --let bin_flip = makeImage row col (\r c -> if (ref bin_e r c) == 0 then 255 else 0) :: GrayImage
  --display bin_flip
  --let l = label $ bin_flip :: GrayImage
  --let a = Data.Vector.Unboxed.toList $ areas l
  --let dimes = numType dimes a
  --let bin = flipBin . erode' $ toBinaryImage (\(RGB (r,g,b)) -> r < 130 && g > 130 && b < 130) coins1
  --let a = Data.Vector.Unboxed.toList $ areas (label bin :: GrayImage)
  --display coins1
  --display bin
  --putStrLn $ show a
  putStrLn $ show (coinValue coins1)
  --  let bin_l = label bin
--  display bin_l
  --planet <- readColorImage "planet.ppm"
  --pred <- readColorImage "pred.ppm"
  --let pred_crop = crop 0 0 305 500 pred
  --let pred_cent = centerImage 961 1024 pred_crop
  --display pred_cent
  --let pred_bin = makeBinary pred_cent [(0,255), (150,210), (0,255)]
  --display pred_bin
  --let pred_bin' = close' pred_bin
  --display pred_bin'
  --putStrLn $ show (dimensions pred)
  --putStrLn $ show (dimensions planet)
  --let composite = makeComposite planet pred_cent [(0,255), (150,210), (0,255)]
  --display composite
  --plotHistograms $ colorImageToRGB pred
