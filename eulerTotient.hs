module EulerTotient where

import Data.Array

divisorList :: Int -> [[Int]]
divisorList n = elems $ accumArray (flip (:)) [] (1, n) [ (y, x) | x <- [1..n], y <- [x,x+x..n] ]

eulerTotientArray :: Int -> Array Int Int
eulerTotientArray n = ret where
    ret = listArray (1, n) $ map f $ divisorList n
    f (x:divs) = x - sum (map (ret!) divs)
