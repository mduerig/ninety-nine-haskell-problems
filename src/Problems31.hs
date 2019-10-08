module Problems31 where

-- Problem 31: Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n = n > 1 && all (\d -> (n `mod` d) /= 0) factors
    where
        factors = takeWhile (\m -> m*m <= n) [2..]


-- Problem 32: Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.
gcd' :: Int -> Int -> Int
gcd' m n = if m < n
    then gcd' n m
    else
        let
            r = m `mod` n
        in
            if r == 0
                then n
                else gcd' n r

-- Problem 33: Determine whether two positive integer numbers are coprime. Two numbers
-- are coprime if their greatest common divisor equals 1.
coPrime :: Int -> Int -> Bool
coPrime m n = gcd' m n == 1

