{-# LANGUAGE ScopedTypeVariables #-}

import Z
import Data.List
import Test.QuickCheck

-- Longest common prefix
lcp xs ys = length $ takeWhile (uncurry (==)) $ zip xs ys

-- Specification for the kmp automaton generator.
zFunctionSpec ls = map (lcp ls) $ init $ tails ls

-- Specification for the string matching function
strMatchSpec str "" = []
strMatchSpec str pat = findIndices (isPrefixOf pat)
                     $ tails str

main = do
    quickCheck (\(xs :: String) -> zFunction xs == zFunctionSpec xs)
    quickCheck (\(xs :: String) (ys :: String) -> strMatch xs ys == strMatchSpec xs ys)
