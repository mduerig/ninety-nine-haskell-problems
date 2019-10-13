module Tests55 where

import Problems55
import Test.QuickCheck
import Data.List

tests55Main :: IO()
tests55Main = do
    quickCheck balTreeNodeCountProp
    quickCheck balTreeProp
    quickCheck mirrorProp
    quickCheck binTreeProp
    quickCheck foldableProp
    quickCheck symBalTreeProp
    quickCheck highBalTreeProp
    quickCheck collectLeavesProp
    quickCheck collectBranchesProp
    quickCheck collectAtLevelProp
    quickCheck completeBinTreeProp

balTreeNodeCountProp :: Positive Int -> Bool
balTreeNodeCountProp (Positive n) = countNodes (balTree n) == n

balTreeProp :: Positive Int -> Bool
balTreeProp (Positive n) = balanced (balTree n)

mirrorProp :: Positive Int -> Bool
mirrorProp (Positive n) =
    let
        tree = balTree n
    in
        tree == (mirror . mirror $ tree)


binTreeProp :: [Int] -> Bool
binTreeProp xs =
    let
        tree = binTree xs
    in
        preOrderNodes tree == sort xs

foldableProp :: [Int] -> Bool
foldableProp xs =
    let
        tree = binTree xs
    in
        foldr (:) [] tree == sort xs

symBalTreeProp :: Positive Int -> Bool
symBalTreeProp (Positive n) =
    let
        tree = symBalTree n
    in
        symmetric tree && balanced tree

highBalTreeProp :: Int -> Positive Int -> Property
highBalTreeProp x (Positive n) = n < 6 ==>
    let
        trees = highBalTree x n
    in
        all balanced trees &&
        all (\t -> height t == n) trees

collectLeavesProp :: [Int] -> Bool
collectLeavesProp xs =
    let
        tree = binTree xs
    in
        length (collectLeaves tree) == countLeaves tree

collectBranchesProp :: [Int] -> Bool
collectBranchesProp xs =
    let
        tree = binTree xs
    in
       length (collectBranches tree) + length (collectLeaves tree) == length xs

collectAtLevelProp :: [Int] -> Property
collectAtLevelProp xs = xs /= [] ==>
    let
        tree = binTree xs
    in
        (sum $ (map (\d -> length $ collectAtLevel (d + 1) tree) [0..height tree])) == length xs

completeBinTreeProp :: Positive Int -> Bool
completeBinTreeProp (Positive n) =
    let
        tree = completeBinTree n
    in
        countNodes tree == n
