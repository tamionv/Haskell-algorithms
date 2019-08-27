{-# LANGUAGE FlexibleInstances #-}

import ConvexHull
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad

instance Arbitrary (Point Int) where
    arbitrary = Point <$> arbitrary <*> arbitrary

-- Rotates ls by n
shift n ls = take (length ls) $ drop n $ cycle ls

-- Gives us the cross products of all adjacent points in ps,
-- with respect to base as an origin coordinate.
crossProductsWith base ps = zipWith (crossProduct base) ps $ shift 1 ps

-- The signed area of a polygon ps.
signedArea ps = sum $ crossProductsWith (Point {x = 0, y = 0}) ps

-- Checks if the hull of ps is in trigonometric order.
hullInTrigoOrder :: [Point Int] -> Bool
hullInTrigoOrder ps = signedArea (convexHull ps) >= 0

-- Checks if p is in polygon ps.
p `inPolygon` ps = signedArea ps == (sum $ map abs $ crossProductsWith p ps)

-- Checks that all of ps is included in the convex hull of ps.
allInConvexHull :: [Point Int] -> Bool
allInConvexHull ps = all (`inPolygon` convexHull ps) ps

-- Checks that a list of points is strictly convex, and in trigonometric order.
isConvex :: [Point Int] -> Bool
isConvex ps = length ps <= 2 || all (<0) (zipWith3 crossProduct ps (shift 2 ps) (shift 1 ps))

main = do
    quickCheck allInConvexHull
    quickCheck hullInTrigoOrder
    quickCheck (isConvex . convexHull)
