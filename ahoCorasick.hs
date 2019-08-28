module AhoCorasick where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Monad

-- Helper functions:
-- Apply function to second element of tuple.
second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

-- Turns a list of key-value pairs to a map, from
-- keys, to lists of values associated with those keys.
fromListRep :: Ord k0 => [(k0, a)] -> M.Map k0 [a]
fromListRep = foldl f (M.empty) where
    f m (k, v) = M.alter (Just . (v:) . fromMaybe []) k m

-- An aho-corasick automaton, with suffix links.
data Automaton a = Node { next :: M.Map a (Automaton a)
                        , suffix :: [Automaton a]
                        , myString :: [a]
                        , indices :: [Int]
                        , depth :: Int
                        }

buildAutomaton :: (Ord a, Eq a, Show a) => [[a]] -> Automaton a
buildAutomaton ls = root where
    root = Node { next     = makeSons root $ zip [0..] ls
                , suffix   = [root]
                , myString = []
                , indices  = []
                , depth    = 0
                }
    findSuffix father k
        | depth father == 0 = [father]
        | otherwise = maybe [root] (\x -> x : suffix x)
                    $ msum
                    $ map (M.lookup k . next)
                    $ suffix father
    makeSons me ls = M.mapWithKey (makeNode me)
                    $ fromListRep
                    $ map (\(x, (y, z)) -> (y, (x, z)))
                    $ catMaybes
                    $ map sequence
                    $ map (second uncons) ls 
    makeNode father k ls = me where
        me = Node { next     = makeSons me ls
                  , suffix   = findSuffix father k
                  , indices  = map fst (filter (null . snd) ls)
                             ++ (indices . head . suffix) me
                  , myString = k : myString father
                  , depth    = 1 + depth father
                  }


-- Follow a link in the automaton:
follow :: Ord a => Automaton a -> a -> Automaton a
follow a k = fromMaybe (last $ suffix a) $ msum $ map (M.lookup k) $ map next $ a:(suffix a)

-- Use the Aho - Corasick automaton to find the first appearance
-- of one of many strings:
dictionaryMatch :: (Eq a, Ord a, Show a) => [[a]] -> [a] -> Maybe (Int, Int)
dictionaryMatch dict str = fmap solutionFromPos position where
    pathIndices = map indices $ scanl follow (buildAutomaton dict) str
    position = findIndex (not . null) pathIndices
    solutionFromPos pos = (which, pos - length (dict!!which)) where
        which = minimum $ pathIndices!!pos
