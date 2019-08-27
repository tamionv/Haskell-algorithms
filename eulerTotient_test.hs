import EulerTotient
import Test.QuickCheck
import Data.Array

divisors x = reverse $ filter ((==0) . (x `mod`)) [1..x]

divisorListProperty :: Int -> Bool
divisorListProperty n = divisorList n == map divisors [1..n]

totient x = length $ filter ((==1) . (gcd x)) [1..x]

eulerTotientArrayProperty :: Int -> Bool
eulerTotientArrayProperty n = elems (eulerTotientArray n) == map totient [1..n]

main = do
    quickCheck divisorListProperty
    quickCheck eulerTotientArrayProperty
