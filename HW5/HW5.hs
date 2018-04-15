{-# LANGUAGE FlexibleContexts #-}
import Data.Image.Boxed
import Data.Image.Interactive
import Data.Image.Internal 
import Data.Image.Binary
import Data.Image.Convolution
import Data.Image.IO
import GHC.Float

fillImage :: Int -> GrayImage -> GrayImage
fillImage r img = makeImage r (snd (dimensions img)) (\row col -> let (r', c') = dimensions img in
                                        if row < r' && col < c' then ref img row col else 0)

displayHelper :: Int -> [GrayImage] -> GrayImage
displayHelper r [img] = fillImage r img
displayHelper r (i:imgs) = leftToRight (fillImage r i) (displayHelper r imgs)

displayLaplacianPyramid :: [GrayImage] -> GrayImage
displayLaplacianPyramid (i:imgs) = 128 +. (leftToRight i (displayHelper (fst (dimensions i)) imgs))

pyramidHelper :: Int -> GrayImage -> [GrayImage]
pyramidHelper 1 img = [img]
pyramidHelper n img = (img - (project (reduce img))):(pyramidHelper (n-1) (reduce img))

laplacianPyramid img = pyramidHelper n img where n = floor (logBase 2 (fromIntegral (fst (dimensions img))))

inverseLaplacianPyramid :: [GrayImage] -> GrayImage
inverseLaplacianPyramid [img] = img
inverseLaplacianPyramid (i:imgs) = i + (project $ (inverseLaplacianPyramid imgs))

reduce :: GrayImage -> GrayImage
reduce img = downsample $ convolveRows kernel (convolveCols kernel img) :: GrayImage
  where kernel = map (1/20 *) [1, 5, 8, 5, 1]

project :: GrayImage -> GrayImage
project img = convolveRows kernel (convolveCols kernel (upsample img))
  where kernel = map (1/10 *) [1, 5 , 8, 5, 1]

main = do
  
  putStrLn "Test"
