module Problems11 where

import Problems1 ( pack, elementCount )

-- Problem 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred
-- as (N E) lists.
--
-- Example:
--   (encode-modified '(a a a a b c c a a d e e e e))
--   ((4 A) B (2 C) (2 A) D (4 E))

data Enc a = Single a | Tuple a Int
    deriving (Show, Eq)

rlEnc' :: Eq a => [a] -> [Enc a]
rlEnc' xs = map enc (pack xs)
    where
        enc [x]      = Single x
        enc xs@(x:_) = Tuple  x (elementCount xs)


-- Problem 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its
-- uncompressed version.
rlDec' :: [Enc a] -> [a]
rlDec' xs = foldr cat [] xs
    where
        cat (Single x)   xs = x:xs
        cat (Tuple  x n) xs = (take n $ repeat x) ++ xs


-- Problem 13: Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by replacing the
-- singleton lists (1 X) by X.
--
-- Example:
--   encodeDirect "aaaabccaadeeee"
--   [Multiple 4 'a',Single 'b',Multiple 2 'c',
--    Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [Enc a]
encodeDirect = foldr abb []
    where
        abb x []                = [Single x]
        abb x (Single y:ys)
                    | x == y    = (Tuple y 2):ys
                    | x /= y    = (Single x):(Single y):ys
        abb x (Tuple y n:ys)
                    | x == y    = (Tuple y (n + 1)):ys
                    | x /= y    = (Single x):(Tuple y n):ys