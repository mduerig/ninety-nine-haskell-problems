module Problems46 where

import Prelude hiding (not, or, and)
import Data.List hiding (or, and)
import Control.Monad

-- Problem 46: Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
-- A logical expression in two variables can then be written as in the following example:
--
--    and(or(A,B),nand(A,B)).
--
-- Now, write a predicate table/3 which prints the truth table of a given logical expression
-- in two variables.

not :: Bool -> Bool
not True  = False
not False = True

and :: Bool -> Bool -> Bool
and True True = True
and _ _       = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _         = True

nand :: Bool -> Bool -> Bool
nand x y = not (and x y)

nor :: Bool -> Bool -> Bool
nor x y = not (or x y)

equ :: Bool -> Bool -> Bool
equ x y = or (and x y) (and (not x) (not y))

xor :: Bool -> Bool -> Bool
xor x y = not (equ x y)

impl :: Bool -> Bool -> Bool
impl x y = if x
    then y
    else True

table :: (Bool -> Bool -> Bool) -> IO()
table f =
    let
        values = [ show x ++ " " ++ show y ++ " " ++ show (f x y) |
                    x <- [False, True],
                    y <- [False, True]]
    in
        putStrLn $ unlines values


-- Problem 47: Truth tables for logical expressions (2).
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to
-- write the logical expression in the more natural way, as in the example:
--
--    A and (A or not B).
--
-- Define operator precedence as usual; i.e. as in Java.
infixl 4 `or`
infixl 6 `and`


-- Problem 48: Truth tables for logical expressions (3).
-- Generalize problem P47 in such a way that the logical expression may contain any number
-- of logical variables. Define table/2 in a way that table(List,Expr) prints the truth
-- table for the expression Expr, which contains the logical variables enumerated in List.
tableLen :: Int -> ([Bool] -> Bool) -> IO()
tableLen n f = putStrLn . unlines $ map toString (truthValues n)
    where
        toString :: [Bool] -> String
        toString bs = concatMap (\b -> show b ++ " ") bs ++ show (f bs)

        truthValues :: Int -> [[Bool]]
        truthValues n = replicateM n [False, True]
        -- truthValues 0 = [[]]
        -- truthValues n = [ q ++ [w] | q <- truthValues (n - 1), w <- [False, True] ]


-- Problem 49: Gray codes.
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
-- For example,
--
--    n = 1: C(1) = ['0','1'].
--    n = 2: C(2) = ['00','01','11','10'].
--    n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
gray :: Int -> [String]
gray 0 = [[]]
gray n =
    let
        codes = [ g:gs | g <- ['0', '1'], gs <- gray (n - 1) ]
        split = splitAt (length codes `div` 2) codes
    in
        fst split ++ reverse (snd split)


-- Problem 50: Huffman codes.
-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms.
-- Example:
--
--    [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)].
--
-- Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word
-- for the symbol S. In our example, the result could be
--
--    Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
--         [hc(a,'01'),...etc.].
--
-- Example in Haskell:
--
--     λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
--     [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
huffman :: [(Char, Int)] -> [(Char, String)]
huffman fs =
    let
        leaves :: [Heap]
        leaves = sort $ map leaf fs

        reduce :: [Heap] -> [Heap]
        reduce (h1:h2:hs) = sort $ (branch h1 h2):hs

        reduceRec :: [Heap] -> [Heap]
        reduceRec [h] = [h]
        reduceRec hs = reduceRec . reduce $ hs

        hTree :: Heap
        hTree = head $ reduceRec leaves

        traverseTree :: Heap -> String -> [(Char, String)]
        traverseTree (Leaf c w) s = [(c, s)]
        traverseTree (Branch _ l r) s =
            traverseTree l (s ++ "0") ++
            traverseTree r (s ++ "1")
    in
        traverseTree hTree ""

data Heap = Leaf Char Int
          | Branch Int Heap Heap
    deriving (Eq, Show)

instance Ord Heap where
    compare h1 h2 = compare (weight h1) (weight h2)

weight :: Heap -> Int
weight (Leaf _ w) = w
weight (Branch w _ _) = w

leaf :: (Char, Int) -> Heap
leaf (c, w) = Leaf c w

branch :: Heap -> Heap -> Heap
branch l r = Branch (weight l + weight r) l r
