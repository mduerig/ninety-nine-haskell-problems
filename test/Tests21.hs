module Tests21 where

import Problems21
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

tests21Main :: IO ()
tests21Main = do
    quickCheck insertAtProp
    quickCheck rangeProp
    quickCheck rndSelectProp
    quickCheck rndDiffProp

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

rndSelectProp :: [Int] -> Property
rndSelectProp xs = forAll (choose (0, length xs)) $ \i ->
    monadicIO $ do
        rnd <- run (rndSelect i xs)
        assert (all (`elem` xs) rnd)


--        rndDiff :: Int -> Int -> IO [Int]
rndDiffProp :: Int -> Property
rndDiffProp m = forAll (choose (0, m)) $ \n ->
    monadicIO $ do
        rnd <- run (rndDiff n m)
        assert ( all (`elem` [1..m]) rnd
              && all (==1) (map length $ group rnd)
               )
