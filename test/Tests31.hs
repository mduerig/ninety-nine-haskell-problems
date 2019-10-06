module Tests31 where

import Test.QuickCheck
import Problems31

tests31Main :: IO ()
tests31Main = do
    quickCheck isPrimeProp

isPrimeProp :: Positive Int -> Positive Int -> Property
isPrimeProp (Positive n) (Positive m) = m > 1 && n > 1 ==>
     not (isPrime (m*n))