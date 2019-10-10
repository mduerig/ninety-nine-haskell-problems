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