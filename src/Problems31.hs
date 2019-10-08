module Problems31 where

import Data.List

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


-- Problem 34: Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive
-- integers r (1 <= r < m) that are coprime to m.
totient :: Int -> Int
totient n = length $ filter (coPrime n) [1..n-1]


-- Problem 35: Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors n = if isPrime n || n == 1
    then [n]
    else
        let
            Just f = find (\m -> n `mod` m == 0) [2..]
        in
            (primeFactors f) ++ (primeFactors (n `div` f))

-- Problem 36: Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
primeFactorCount :: Int -> [(Int, Int)]
primeFactorCount n =
    let f = group $ primeFactors n
    in zip (map head f) (map length f)


-- Problem 37: Calculate Euler's totient function phi(m) (improved).
-- See problem 34 for the definition of Euler's totient function. If the list of
-- the prime factors of a number m is known in the form of problem 36 then the
-- function phi(m) can be efficiently calculated as follows:
-- Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities)
-- of a given number m. Then phi(m) can be calculated with the following formula:
--
--    phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--             (p2 - 1) * p2 ** (m2 - 1) *
--             (p3 - 1) * p3 ** (m3 - 1) * ...
totient' :: Int -> Int
totient' n = product $ map f (primeFactorCount n)
    where
        f (p, m) = (p - 1) * p ^ (m - 1)
