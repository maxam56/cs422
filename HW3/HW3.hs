{-# LANGUAGE FlexibleContexts #-}
import Data.Image.Boxed hiding (label, dinstanceTransform)
import Data.Image.Interactive
import Data.Image.Internal 
import Data.Image.Binary

biLinear :: GrayImage -> Double -> Double -> Double
biLinear img x y = xVal1-((xVal1-xVal2)*dy)
  where x1 = floor x
        y1 = floor y
        dx = x - (fromIntegral x1)
        dy = y - (fromIntegral y1)
        x2 = x1 + 1
        y2 = y1 + 1
        p1 = ref img y1 x1
        p2 = ref img y1 x2
        p3 = ref img y2 x1
        p4 = ref img y2 x2
        xVal1 = p1 - ((p1-p2)*dx)
        xVal2 = p3 - ((p1-p2)*dx) 

cRef' img x y = (RGB (biLinear imgR x y,biLinear imgG x y,biLinear imgB x y))
  where (imgR,imgG, imgB) = colorImageToRGB img

rotateImage :: ColorImage -> Int -> ColorImage
rotateImage img deg = makeImage rows cols (\r c -> let x' = c - xCent
                                                       y' = r - yCent
                                                       x'' = (fromIntegral $ x') * (cos rad) - (fromIntegral $ y') * (sin rad)
                                                       y'' = (fromIntegral $ x') * (sin rad) + (fromIntegral $ y') * (cos rad) in
                                                     if (floor x'') + xCent > 0 && (floor x'') + xCent <= cols && (floor y'') + yCent > 0 && (floor y'') + yCent <= rows
                                                     then (RGB ((ref' imgR (y''+(fromIntegral $ yCent-1)) (x''+(fromIntegral $ xCent-1))),
                                                                (ref' imgG (y''+(fromIntegral $ yCent-1)) (x''+(fromIntegral $ xCent-1))),
                                                                (ref' imgB (y''+(fromIntegral $ yCent-1)) (x''+(fromIntegral $ xCent-1))))) else 0)
                                                          
--                                                     then ref img ((floor y'') + yCent - 1) ((floor x'') + xCent - 1) else 0)
                                                     
  where (rows, cols) = dimensions img
        rad = (fromIntegral deg) * ((pi :: Double)/ 180)
        xCent = cols `div` 2
        yCent = rows `div` 2
        (imgR,imgG,imgB) = colorImageToRGB img 

coneToPanorama :: ColorImage -> ColorImage
coneToPanorama img = makeImage center (floor $ width) (\r c -> let theta = (fromIntegral c) / (fromIntegral center)
                                                                   radius = (fromIntegral r)
                                                                   y = (fromIntegral center) + radius * (cos theta)
                                                                   x = (fromIntegral center) + radius * (sin theta) in (RGB (biLinear imgR x y, biLinear imgG x y, biLinear imgB x y)))
  where (rows, cols) = dimensions img
        center = cols `div` 2
        width = 2 * (pi :: Double) * (fromIntegral center)
        (imgR,imgG,imgB) = colorImageToRGB img

main = do
 -- mona <- readColorImage "louvre-small.ppm"
 -- display mona
 -- putStrLn "Test"
  cone <- readColorImage "cone-mirror.ppm"
  display cone
  let coneP = coneToPanorama cone
  display coneP
