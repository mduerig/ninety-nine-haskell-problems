module Problems31 where

-- Problem 31: Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n = n > 1 && all (\d -> (n `mod` d) /= 0) factors
    where
        factors = takeWhile (\m -> m*m <= n) [2..]


