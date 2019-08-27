module Z where

import Data.Array
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy

-- The function takes a list of values, comparable by equality, and
-- for each suffix, finds the length of the longest prefix of that suffix
-- which is also a prefix of the entire list.
zFunction :: Eq a => [a] -> [Int]
zFunction str = elems ret where
    n = length str
    arr = listArray (1, n) str
    ret = listArray (1, n) $ n : evalState (mapM getValue [2..]) 1
    getValue i = do
        prev <- get
        let start | prev == 1 || prev + ret!prev <= i = 0
                  | otherwise = min (prev + ret!prev - i) (ret!(i - prev + 1))
        let isGood x = arr!(x + 1) == arr!(i + x)
        let value = maximum $ 0 : map (+1) (takeWhile isGood [start..n - i])
        when (i + value > prev + ret!prev) $ put i
        return $ value

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
         $ zFunction buf
