module ConvexHull where

import Data.List

-- A generic point type
data Point a = Point { x :: a, y :: a } deriving (Eq, Show)

-- The cross product of vectors p1-base and p2-base
crossProduct :: Num a => Point a -> Point a -> Point a -> a
crossProduct base p1 p2 = x p1 * y p2
                        + x p2 * y base
                        + x base * y p1
                        - x p2 * y p1
                        - x base * y p2
                        - x p1 * y base

-- The convex hull of a list of points, in reverse trigonometric order.
convexHull :: (Ord a, Num a) => [Point a] -> [Point a]
convexHull [] = []
convexHull xs = init lowerHull ++ init upperHull where
    sortedXs = sortBy (\p q -> compare (x p, y p) (x q, y q)) xs
    lowerHull = getHull sortedXs
    upperHull = getHull $ reverse sortedXs
    getHull = reverse . foldl advanceHull []
    advanceHull st p | length st <= 1 || crossProduct (st!!0) (st!!1) p < 0 = p:st
                     | otherwise = advanceHull (tail st) p
