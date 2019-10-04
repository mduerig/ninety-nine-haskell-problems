module Problems21 where

import System.Random

-- Problem 21: Insert an element at a given position into a list.
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)


-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i j
    | i >  j = []
    | i == j = [i]
    | i <  j = i:(range (i + 1) j)


-- Problem 23: Extract a given number of randomly selected elements from a list.
rndSelect :: Int -> [a] -> IO [a]
rndSelect _ [] = return []
rndSelect n xs = do
    rnd <- getStdGen
    return $ take n [ xs!!i | i <- randomRs (0, (length xs) - 1) rnd ]
    -- getStdGen >>= (\rnd -> return $ take n [ xs!!i | i <- randomRs (0, (length xs) - 1) rnd ])
    -- fmap (\rnd -> take n [ xs!!i | i <- randomRs (0, (length xs) - 1) rnd ]) getStdGen

-- Problem 24: Lotto: Draw N different random numbers from the set 1..M.
rndDiff :: Int -> Int -> IO [Int]
rndDiff n m = do
    rnd <- getStdGen
    let r = foldr abb ([], rnd) [(m - n + 1)..m]
    return $ fst r

    where
        insert :: Int -> [Int] -> [Int]
        insert x xs
             | x `elem` xs = insert (x + 1) xs
             | otherwise   = x:xs

        abb :: Int -> ([Int], StdGen) -> ([Int], StdGen)
        abb k (rs, rnd) =
            let (r', rnd') = randomR (1, k) rnd
            in  (insert r' rs, rnd')