module Problems21 where

-- Problem 21: Insert an element at a given position into a list.
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)


-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i j
    | i >  j = []
    | i == j = [i]
    | i <  j = i:(range (i + 1) j)


