module Tests31 where

import Test.QuickCheck
import Problems31
import Data.List

tests31Main :: IO ()
tests31Main = do
    quickCheck isPrimeProp
    quickCheck gcd'Prop
    quickCheck coPrimeProp
    quickCheck totientProp
    quickCheck primeFactorsSortedProp

isPrimeProp :: Positive Int -> Positive Int -> Property
isPrimeProp (Positive n) (Positive m) = m > 1 && n > 1 ==>
     not (isPrime (m*n))

gcd'Prop :: Positive Int -> Positive Int -> Bool
gcd'Prop (Positive m) (Positive n) = gcd m n == gcd' m n

coPrimeProp :: Positive Int -> Positive Int -> Bool
coPrimeProp (Positive m) (Positive n) = (gcd m n == 1) == coPrime m n

totientProp :: Int -> Property
totientProp n = isPrime n ==>
    totient n == n -1

primeFactorsSortedProp :: Positive Int -> Bool
primeFactorsSortedProp (Positive n) =
    let f = primeFactors n
    in f == (sort f)

primeFactorsProp :: Int -> Bool
primeFactorsProp n = (product $ primeFactors n) == n