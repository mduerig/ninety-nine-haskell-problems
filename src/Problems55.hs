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


-- Problem 56:  Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a vertical line through the root
-- node and then the right subtree is the mirror image of the left subtree. Write a predicate
-- symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate
-- mirror/2 first to check whether one tree is the mirror image of another. We are only
-- interested in the structure, not in the contents of the nodes.
symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = l == mirror r

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch x l r) = Branch x (mirror r) (mirror l)