-- This module defines a function that finds the longest
-- common subsequence of two lists.
module LCS where

import Data.Array
import Data.Bifunctor

lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys = reverse $ fst $ ret!(n, m) where
    n = length xs
    m = length ys
    xsArr = listArray (1, n) xs
    ysArr = listArray (1, n) ys
    rec a b | a == 0 || b == 0 = ([], 0 :: Int)
            | xsArr!a == ysArr!b = bimap (xsArr!a:) (+1) $ ret!(a-1, b-1)
            | snd (ret!(a-1, b)) < snd (ret!(a, b-1)) = ret!(a, b-1)
            | otherwise = ret!(a-1, b)
    ret = array ((0, 0), (n, m)) [((i, j), rec i j) | i <- [0..n], j <- [0..m]]
