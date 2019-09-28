module Tests21 where

import Problems21
import Test.QuickCheck

tests21Main :: IO ()
tests21Main = do
    quickCheck insertAtProp
    quickCheck rangeProp

insertAtProp :: Int -> [Int] -> Property
insertAtProp x [] = property $ (insertAt 1 x []) == [x]
insertAtProp x xs = forAll (choose (1, length xs)) $ \i ->
    let
        xs' = insertAt i x xs
    in
        xs'!!(i - 1)     == x &&
        take (i - 1) xs' == take (i - 1) xs &&
        drop i xs'       == drop (i - 1) xs

rangeProp :: Int -> Int -> Bool
rangeProp i j = range i j == [i..j]