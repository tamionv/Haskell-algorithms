import AhoCorasick
import KMP
import Data.Maybe
import Data.List
import Test.QuickCheck
import Control.Monad

safeHead [] = Nothing
safeHead (x:_) = Just x

revCompare (p, q) (s, t) = compare (q, p) (t, s)

bruteForceDictionaryMatch :: (Eq a, Ord a) => [[a]] -> [a] -> Maybe (Int, Int)
bruteForceDictionaryMatch dict str = safeHead
                                   $ map (\(x, y) -> (x, y - length (dict!!x)))
                                   $ sortBy revCompare
                                   $ map (\(x, y) -> (x, y + length (dict!!x)))
                                   $ catMaybes
                                   $ zipWith (\x -> fmap (\t -> (x,t))) [0..]
                                   $ map (safeHead . strMatch str) dict

dictionaryMatchProp :: [[Bool]] -> [Bool] -> Bool
dictionaryMatchProp dict str = (bruteForceDictionaryMatch dict str == dictionaryMatch dict str)

main = do
    quickCheck (withMaxSuccess 10000 dictionaryMatchProp)
