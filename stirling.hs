-- Module to find stirling numbers
module Stirling where

import Data.Array

-- Makes an array that contains the stirling numbers of the first kind.
stirling1Array :: Num a => Int -> Int -> Array (Int, Int) a
stirling1Array n k = ret where
    ret = array ((0, 0), (n, k)) [((i, j), f i j) | i <- [0..n], j <- [0..k]]
    f i j | i == 0 && j == 0 = 1
          | i == 0 || j == 0 = 0
          | otherwise = (fromIntegral $ i - 1) * ret!(i - 1, j) + ret!(i - 1, j - 1)

-- Makes an array that contains the stirling numbers of the second kind.
stirling2Array :: Num a => Int -> Int -> Array (Int, Int) a
stirling2Array n k = ret where
    ret = array ((0, 0), (n, k)) [((i, j), f i j) | i <- [0..n], j <- [0..k]]
    f i j | i == 0 && j == 0 = 1
          | i == 0 || j == 0 = 0
          | otherwise  = (fromIntegral j) * ret!(i - 1, j) + ret!(i - 1, j - 1)

-- Retrieves a stirling number of the first kind.
stirling1 n k = (stirling1Array n k)!(n, k)

-- Retrieves a stirling number of the second kind.
stirling2 n k = (stirling2Array n k)!(n, k)
