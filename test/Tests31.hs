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
    quickCheck totient'Prop
    quickCheck primesInRangeProp
    quickCheck goldbachProp

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

totient'Prop :: Positive Int -> Bool
totient'Prop (Positive n) = totient n == totient' n


primesInRangeProp :: Positive Int -> Positive Int -> Property
primesInRangeProp (Positive p) (Positive q) = p < q ==>
    let
        r = primesInRange p q
    in
        r == [] || (
        p <= minimum r &&
        q >= maximum r &&
        all isPrime r )

goldbachProp :: Int -> Property
goldbachProp n = n > 2 && n `mod` 2 == 0 ==>
    let
        (p, q) = goldbach n
    in
        isPrime p &&
        isPrime q &&
        p + q == n