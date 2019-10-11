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

balTreeNodeCountProp :: Positive Int -> Bool
balTreeNodeCountProp (Positive n) = countNodes (balTree n) == n

balTreeProp :: Positive Int -> Bool
balTreeProp (Positive n) =
    let
        tree = balTree n

        balanced :: Tree a -> Bool
        balanced Empty = True
        balanced (Branch _ l r) =
            (abs ((countNodes l) - (countNodes r)) <= 1) &&
            balanced l &&
            balanced r
    in
        balanced tree


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