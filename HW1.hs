{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Image
import Data.Image.Interactive
import Data.Image.Boxed
import Data.Array.IArray
import Data.Image.Internal
import qualified Data.Vector.Unboxed (toList)
import qualified Data.Vector (toList, fromList, (!), Vector)

--firstEqual :: [(Int,Int)] -> Int -> [(Int, Int)]
firstEqual [] _ = []
firstEqual (t@(a,b):xs) n = if a == n then t:(firstEqual xs n) else firstEqual xs n

interpHelper [t] 0 = [t]
interpHelper [t] n = if (fst t) == n then t:(interpHelper [(n-1,(snd t) `div` 2)] (n-1)) else (n,(snd t) `div` 2):(interpHelper [t] (n-1))

interpHelper (t1:ts) n = if f == n then t1:(interpHelper ts n) else if f == (n-1) then t1:(interpHelper ts (n-1)) else t1:(interpHelper (newTup:ts)  (n-1))
  where t2 = head ts
        f = fst t2
        newTup = (n-1,snd t2)
        --newTup = (n-1,((snd t1)+(snd t2)) `div` 2)
interp ts = interpHelper ts 100 

--getInvCdfHelper :: GrayImage -> [(Int,Int)]
getInvCdfHelper img = interp $ [(100 - x,y) | x <- [0..100], y <- [0..255], (floor $ (cdf !! y) * 100) == x] ++ [(0,255)]
  where cdf = Data.Vector.toList $ getCdf img

getInvCdf img = Data.Vector.fromList $ reverse $ map fromIntegral $ [(sum (map snd p)) `div` (length p) | x <- [0..100], let p = firstEqual pairs x]
  where pairs = getInvCdfHelper img

histMatch :: GrayImage -> GrayImage -> GrayImage
histMatch img1 img2 = imageMap (\x -> invCdf !! (round $ 100 * (cdf !! (round x)))) img1
  where cdf = Data.Vector.toList $ getCdf img1
        invCdf = Data.Vector.toList $ getInvCdf img2
        
--getCdf :: (Image img) => img -> Data.Vector.Vector Double
getCdf img = Data.Vector.fromList $ [sum (take n pdf) | n <- [0..255]]
  where pdf = getPdf img
  
getPdf img = map (/ a)  hist
  where hist = Data.Vector.Unboxed.toList $ areas img
        a = fromIntegral $ (rows img) * (cols img)

histEqualize :: GrayImage -> GrayImage
histEqualize img = imageMap (\x -> 255 * (cdf !! (floor x))) img
  where cdf = Data.Vector.toList $ getCdf img

  
main = do
  frog <- readImage "frog.pgm"
  rad <- readImage "radiation.pgm"
  --let pdf = getPdf frog
  --let frogHist = areas frog
  --putStrLn $ show (sum pdf)
  --let cdf = getCdf frog
  --putStrLn $ show (cdf Data.Vector.! 255)
  --plotHistograms $ [frog]
  let eqFrog = histEqualize frog
  plotHistograms $ [frog, eqFrog]
  display frog
  display eqFrog
