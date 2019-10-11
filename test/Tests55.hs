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