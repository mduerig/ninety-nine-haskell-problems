module Problems21 where

import System.Random
import Data.List.HT ( rotate )
import Data.List
import Control.Monad

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


-- Problem 25: Generate a random permutation of the elements of a list.
rndPerm :: [a] -> IO [a]
rndPerm xs = rndComb (length xs) xs


rndComb :: Int -> [a] -> IO [a]
rndComb k xs = do
    ix  <- rndDiff k (length xs)
    return $ get ix xs

    where
        get :: [Int] -> [a] -> [a]
        get []     _  = []
        get (i:ix) xs = (xs!!(i - 1)):(get ix xs)


perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = permute =<< rotations
    where
        permute :: [a] -> [[a]]
        permute (x:xs) = map (x:) (perms xs)

        len            = length xs
        rotations      = take len $ iterate (rotate (len - 1)) xs


-- Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list
-- combinations :: Int -> [a] -> [[a]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           ,   ys  <- combinations (n - 1) xs' ]


-- Problem 27: Group the elements of a set into disjoint subsets.
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
-- Write a function that generates all the possibilities and returns them in a list.
group3 :: Eq a => [a] -> [[[a]]]
group3 xs = [ [g2, g3, g4] | g2 <- combinations 2 xs
                           , g3 <- combinations 3 (xs \\ g2)
                           , g4 <- combinations 4 ((xs \\ g2) \\ g3) ]

-- b) Generalize the above predicate in a way that we can specify a list of group sizes and
-- the predicate will return a list of groups.
groupN :: Eq a => [Int] -> [a] -> [[[a]]]
groupN []     xs = [[]]
groupN (c:cs) xs = [ g:gs | g   <- combinations c xs
                          , gs  <- groupN cs (xs \\ g) ]


-- Problem 28: Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves. The objective is to
-- sort the elements of this list according to their length. E.g. short lists first, longer
-- lists later, or vice versa.
lSort :: [[a]] -> [[a]]
lSort = sortOn length
--lSort = sortBy (\x y -> compare (length x) (length y))


-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their
-- length frequency; i.e., in the default, where sorting is done ascendingly, lists with
-- rare lengths are placed first, others with a more frequent length come later.
lfSort :: [[a]] -> [[a]]
lfSort xs =
    let
        frequencies = join $ map nub (sortOn length (group . sort $ map length xs))
        freq x = elemIndex (length x) frequencies
    in
        sortOn freq xs
