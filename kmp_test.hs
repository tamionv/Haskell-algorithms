{-# LANGUAGE ScopedTypeVariables #-}

import KMP
import Data.List
import Test.QuickCheck

-- Specification for the kmp automaton generator.
kmpAutomatonSpec ls = map f $ tail $ inits ls where
    f x = reverse
        $ findIndices id
        $ zipWith (==) (inits x)
        $ reverse
        $ tails
        $ tail x

-- Specification for the string matching function
strMatchSpec str "" = []
strMatchSpec str pat = findIndices (isPrefixOf pat)
                     $ tails str

main = do
    quickCheck (\(xs :: String) -> kmpAutomaton xs == kmpAutomatonSpec xs)
    quickCheck (\(xs :: String) (ys :: String) -> strMatch xs ys == strMatchSpec xs ys)
