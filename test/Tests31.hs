module Tests31 where

import Test.QuickCheck
import Problems31

tests31Main :: IO ()
tests31Main = do
    quickCheck isPrimeProp
    quickCheck gcd'Prop

isPrimeProp :: Positive Int -> Positive Int -> Property
isPrimeProp (Positive n) (Positive m) = m > 1 && n > 1 ==>
     not (isPrime (m*n))

gcd'Prop :: Positive Int -> Positive Int -> Bool
gcd'Prop (Positive m) (Positive n) = gcd m n == gcd' m n