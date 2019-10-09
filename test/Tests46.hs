module Tests46 where

import Test.QuickCheck
import Problems46
import Prelude(Bool(True, False), IO, (&&), (==))

tests46Main :: IO ()
tests46Main = do
    quickCheck andProp
    quickCheck orProp
    quickCheck notProp
    quickCheck nandProp
    quickCheck norProp
    quickCheck equXorProp
    quickCheck implProp
    quickCheck infixProp

andProp :: Bool -> Bool
andProp x = and x True  == x
         && and x False == False
         && and True  x == x
         && and False x == False

orProp :: Bool -> Bool
orProp x = or x True  == True
        && or x False == x
        && or True  x == True
        && or False x == x

notProp :: Bool -> Bool
notProp x = not (not x) == x

nandProp :: Bool -> Bool -> Bool
nandProp x y = nand x y == not (and x y)

norProp :: Bool -> Bool -> Bool
norProp x y = nor x y == not (or x y)

equXorProp :: Bool -> Bool -> Bool
equXorProp x y = equ x y == not (xor x y)
              && xor x y == not (equ x y)

implProp :: Bool -> Bool -> Bool
implProp x y = impl x y == or (not x) y

infixProp :: Bool -> Bool -> Bool -> Bool
infixProp x y z = (x `or` y `and` z) == or x (and y z)