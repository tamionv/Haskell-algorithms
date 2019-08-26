module KMP where

import Data.Array
import Data.List
import Data.Maybe

-- The function takes a list of values, comparable by equality, and
-- for each prefix, finds the lengths of the prefixes equal to the
-- suffixes of that prefix.
kmpAutomaton :: Eq a => [a] -> [[Int]]
kmpAutomaton [] = []
kmpAutomaton str = elems ret where
    n = length str
    arr = listArray (1, n) str
    ret = listArray (1, n) $ [0] : map getRet [2..]
    getRet i = maybe [0] (\x -> x : ret!x)
              $ find (\x -> arr!x == arr!i)
              $ map (+1)
              $ ret!(i-1)

-- This function takes a string and a pattern, and returns
-- the indices in the string at which the pattern apears.
strMatch :: Eq a => [a] -> [a] -> [Int]
strMatch _ [] = []
strMatch str pat = map (\x -> x - m + 1) ends where
    n = length str
    m = length pat
    buf = map Just pat ++ [Nothing] ++ map Just str
    ends = findIndices (==m)
         $ drop (m+1)
         $ map head
         $ kmpAutomaton buf
