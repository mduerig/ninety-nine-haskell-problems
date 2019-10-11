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

balanced :: Tree a -> Bool
balanced Empty = True
balanced (Branch _ l r) =
    (abs ((countNodes l) - (countNodes r)) <= 1) &&
    balanced l &&
    balanced r


mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch x l r) = Branch x (mirror r) (mirror l)


-- Problem 57: Binary search trees (dictionaries)
-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to
-- construct a binary search tree from a list of integer numbers.
binTree :: [Int] -> Tree Int
binTree []     = Empty
binTree (n:ns) = insertNode n (binTree ns)
    where
        insertNode :: Int -> Tree Int -> Tree Int
        insertNode n Empty = leaf n
        insertNode n (Branch m l r)
                          | n < m     = Branch m (insertNode n l) r
                          | otherwise = Branch m l (insertNode n r)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Branch x l r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

preOrderNodes :: Tree a -> [a]
preOrderNodes Empty = []
preOrderNodes (Branch x l r) = (preOrderNodes l) ++ (x:(preOrderNodes r))


-- Problem 58: Generate-and-test paradigm
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced
-- binary trees with a given number of nodes.
symBalTree :: Int -> Tree String
symBalTree 0 = Empty
symBalTree n = (Branch "x" t (mirror t))
    where
        t = balTree ((n - 1) `div` 2)

height :: Tree a -> Int
height Empty = 0
height (Branch _ Empty Empty) = 0
height (Branch _ l r) = 1 + max (height l) (height r)

-- Problem 59: Construct height-balanced binary trees
-- In a height-balanced binary tree, the following property holds for every node:
-- The height of its left subtree and the height of its right subtree are almost equal,
-- which means their difference is not greater than one.
-- Construct a list of all height-balanced binary trees with the given element and the
-- given maximum height.
highBalTree :: a -> Int -> [Tree a]
highBalTree x 0 = [ Empty ]
highBalTree x 1 = [ Branch x Empty (leaf x), Branch x (leaf x) Empty ]
highBalTree x n = [ Branch x l r | l <- highBalTree x (n - 1), r <- highBalTree x (n - 1) ]


-- Problem 61: Count the leaves of a binary tree
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Problem 61A: Collect the leaves of a binary tree in a list
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect
-- them in a list.
collectLeaves :: Tree a -> [Tree a]
collectLeaves Empty = []
collectLeaves t@(Branch _ Empty Empty) = [t]
collectLeaves (Branch _ l r) = collectLeaves l ++ collectLeaves r
