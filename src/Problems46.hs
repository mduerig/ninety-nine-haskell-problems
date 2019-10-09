module Problems46 where

import Prelude ( Bool(False, True), IO, putStrLn, show, ($), unlines)
import Data.List ((++))

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

