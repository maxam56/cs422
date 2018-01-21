{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Image
import Data.Image.Interactive
import Data.Image.Boxed
import Data.Array.IArray
import Data.Image.Internal

--3
flipImage img = makeImage row col (\r c -> ref img (row-r-1) (col-c-1)) where (row,col) = dimensions img

--5
flag h = makeImage h w  (\r c -> if c <= min then RGB (0,0,1) else if c <= 2*min then RGB (1,1,1) else RGB (1,0,0))
  where w = h * 4 `div` 3
        min = w `div` 3
--6
colorImageCrop i0 j0 m n img = makeImage m n (\i j -> ref img (i+i0) (j+j0))

--7
colorImgMerge cim0 cim1 = hsiToColorImage (h1, s1, i0)
  where (_, _, i0) = colorImageToHSI cim0
        (h1, s1, _) = colorImageToHSI cim1

--8 
reduceRows img = downsampleRows $ convolveRows [0.05, 0.25, 0.4, 0.25, 0.05] img
reduceCols img = downsampleCols $ convolveCols [0.05, 0.25, 0.4, 0.25, 0.05] img

rightSplit img 0 = img
rightSplit img n = leftToRight newSplit (rightSplit down (n-1))
  where newSplit = reduceRows img
        down = topToBottom (downsample img) (downsample img)
        
bottomSplit img 0 = img
bottomSplit img n = topToBottom newSplit (bottomSplit down (n-1))
  where newSplit = reduceCols img
        down = leftToRight (downsample img) (downsample img)

cornerSplit img 0 = img
cornerSplit img n = topToBottom (leftToRight tl tr) (leftToRight bl br)
  where tl = downsample img
        rSplit = reduceCols $ rightSplit img n
        bSplit = reduceRows $ bottomSplit img n
        (_, rSplitCols) = dimensions rSplit
        (bSplitRows, _) = dimensions bSplit
        tr = crop 0 cols cols cols rSplit where cols = rSplitCols `div` 2
        bl = crop rows 0 rows rows bSplit where rows = bSplitRows `div` 2
        br = downsample $ cornerSplit img (n-1)
        
main = do
  frog <- readImage "frog.pgm"
  cact <- readColorImage "cactii.ppm"
  lisaP <- readColorImage "prado-small.ppm"
  lisaL <- readColorImage "louvre-small.ppm"
  house <- readImage "im1i8.pgm"

  let c1 = upsample $ cornerSplit house 1
  let c2 = upsample $ cornerSplit house 2
  let c3 = upsample $ cornerSplit house 3

  display c1
  display c2
  display c3


