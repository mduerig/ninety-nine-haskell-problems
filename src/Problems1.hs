module Problems1 where

-- Problem 1
-- Find the last element of a list.
lastElem :: [a] -> a
lastElem [x]   = x
lastElem (x:xs) = lastElem xs

-- Problem 2
-- Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Int -> [a] -> a
elementAt 1 (x:_) = x
elementAt n (x:xs) = elementAt (n - 1) xs

-- Problem 4
-- Find the number of elements of a list.
elementCount :: [a] -> Int
elementCount [] = 0
elementCount (x:xs) = 1 + (elementCount xs)

-- Problem 5
-- Reverse a list.
reverseList :: [a] -> [a]
reverseList xs =
  let
    reverse rs []  = rs
    reverse rs (x:xs) = reverse (x:rs) xs
  in
    reverse [] xs

-- Problem 6
-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverseList xs)

-- Problem 7: Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list
-- with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
    deriving Show

flattenList :: NestedList a -> [a]
flattenList (Elem x) = [x]
flattenList (List []) = []
flattenList (List (x:xs)) = (flattenList x) ++ (flattenList (List xs))

-- Problem 8: Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy
-- of the element. The order of the elements should not be changed.
dedupList :: Eq a => [a] -> [a]
dedupList [] = []
dedupList [x] = [x]
dedupList (x:y:xs)
    | x == y     = dedupList (x:xs)
    | otherwise  = x:(dedupList (y:xs))

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated
-- elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = pack' [] xs
    where
        pack' ps []          = [ps]
        pack' [] (x:xs)      = pack' [x] xs
        pack' (p:ps) (x:xs)
            | p == x         = pack' (p:x:ps) xs
            | otherwise      = (p:ps):(pack' [] (x:xs))

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called
-- run-length encoding data compression method. Consecutive duplicates of elements are encoded as
-- lists (N E) where N is the number of duplicates of the element E.
rlEnc :: Eq a => [a] -> [(a, Int)]
rlEnc xs = map enc (pack xs)
    where enc xs@(x:_) = (x, elementCount xs)