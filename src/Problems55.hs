module Problems55 where

data Tree a = Empty
          | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = (Branch x) Empty Empty

-- Problem 55: Construct completely balanced binary trees
-- In a completely balanced binary tree, the following property holds for every node:
-- The number of nodes in its left subtree and the number of nodes in its right subtree are
-- almost equal, which means their difference is not greater than one.
-- Write a function cbal-tree to construct completely balanced binary trees for a given
-- number of nodes. The predicate should generate all solutions via backtracking.
-- Put the letter 'x' as information into all nodes of the tree.
balTree :: Int -> Tree String
balTree 0 = Empty
balTree n = Branch "x"
                (balTree ((n - 1) `div` 2))
                (balTree ((n - 1) - ((n - 1) `div` 2)))

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)