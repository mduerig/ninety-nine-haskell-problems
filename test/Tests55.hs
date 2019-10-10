module Tests55 where

import Problems55
import Test.QuickCheck

tests55Main :: IO()
tests55Main = do
    quickCheck balTreeNodeCountProp
    quickCheck balTreeProp

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