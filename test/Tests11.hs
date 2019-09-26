module Tests11 where

import Problems11
import Control.Monad ( liftM, liftM2 )
import Data.List
import Data.List.Split
import Test.QuickCheck

tests11Main :: IO ()
tests11Main = do
    quickCheck rlEncDecProp
    quickCheck rlDecEncProp
    quickCheck encodeDirectProp
    quickCheck dupProp
    quickCheck replProp
    quickCheck dropEveryProp
    quickCheck splitProp
    quickCheck sliceProp

rlEncDecProp :: [Int] -> Bool
rlEncDecProp xs = (rlDec' $ rlEnc' xs) == xs

instance Arbitrary a => Arbitrary (Enc a) where
    arbitrary = oneof [ liftM  Single arbitrary
                      , liftM2 Tuple arbitrary ( suchThat arbitrary (>1) )
                      ]
-- (rlEnc' $ rlDec' xs) on the right side of the equality ensures we reach
-- a normal form. E.g. [Single 1,Tuple 1 3] == [Tuple 1 4]
rlDecEncProp :: [Enc Int] -> Bool
rlDecEncProp xs = (rlEnc' $ rlDec' $ rlEnc' $ rlDec' xs) == (rlEnc' $ rlDec' xs)

encodeDirectProp :: [Int] -> Bool
encodeDirectProp xs = (rlEnc' xs) == (encodeDirect xs)

dupProp :: [Int] -> Bool
dupProp xs = (second $ dup xs) == xs
    where
        second (x:y:xs) = y:(second xs)
        second _        = []

replProp :: Int -> [Int] -> Property
replProp n xs = n > 0 ==>
    map head (chunksOf n $ repl n xs) == xs

dropEveryProp :: Int -> [Int] -> Property
dropEveryProp n xs = n > 0 ==>
    concatMap (take (n - 1)) (chunksOf n xs) == dropEvery n xs

splitProp :: Int -> [Int] -> Property
splitProp n xs = n >= 0 ==>
    (take n xs, drop n xs) == Problems11.split n xs

sliceProp :: [Int] -> Property
sliceProp xs = forAll (choose (1, length xs)) $ \i ->
    forAll (choose (i, length xs)) $ \j ->
       length (slice i j xs) == min (length xs) (j - i + 1)