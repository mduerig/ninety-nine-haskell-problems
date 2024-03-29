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
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

-- Problem 61A: Collect the leaves of a binary tree in a list
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect
-- them in a list.
collectLeaves :: Tree a -> [Tree a]
collectLeaves Empty                    = []
collectLeaves t@(Branch _ Empty Empty) = [t]
collectLeaves (Branch _ l r)           = collectLeaves l ++ collectLeaves r


-- Problem 62: Collect the internal nodes of a binary tree in a list
-- An internal node of a binary tree has either one or two non-empty successors.
-- Write a predicate internals/2 to collect them in a list.
collectBranches :: Tree a -> [Tree a]
collectBranches Empty                  = []
collectBranches (Branch _ Empty Empty) = []
collectBranches t@(Branch _ l r)       = t : collectBranches l ++ collectBranches r


-- Problem 62B: Collect the nodes at a given level in a list
-- A node of a binary tree is at level N if the path from the root to the node has
-- length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all
-- nodes at a given level in a list.
collectAtLevel :: Int -> Tree a -> [Tree a]
collectAtLevel n t = collect 1 t
    where
        collect _ Empty          = []
        collect d t | d == n     = [t]
        collect d (Branch _ l r) = (collect (d + 1) l) ++ (collect (d + 1) r)

left :: Tree a -> Tree a
left Empty = Empty
left (Branch _ l _) = l

right :: Tree a -> Tree a
right Empty = Empty
right (Branch _ _ r) = r

-- Problem 63: Construct a complete binary tree
-- A complete binary tree with height H is defined as follows:
-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes
-- are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come
-- first, the leaves come second, and empty successors (the nil's which are not really nodes!)
-- come last.
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for
-- heaps. We can assign an address number to each node in a complete binary tree by enumerating
-- the nodes in level-order, starting at the root with number 1. For every node X with address A
-- the following property holds: The address of X's left and right successors are 2*A and 2*A+1,
-- respectively, if they exist. This fact can be used to elegantly construct a complete binary
-- tree structure.
-- Write a predicate complete_binary_tree/2.
completeBinTree :: Int -> Tree String
completeBinTree n = createNode 1
    where
        createNode k
            | k > n     = Empty
            | otherwise = Branch "x" (createNode (k * 2)) (createNode (k * 2 + 1))


-- Problem 64
-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing
-- the tree, a layout algorithm is required to determine the position of each node in a rectangular
-- grid. Several layout methods are conceivable, one of them is shown in the illustration below:
-- In this layout strategy, the position of a node v is obtained by the following two rules:
--     x(v) is equal to the position of the node v in the inorder sequence
--     y(v) is equal to the depth of the node v in the tree
-- Write a function to annotate each node of the tree with a position, where (1,1) in the top left
-- corner or the rectangle bounding the drawn tree.
data Weighted a = Weighted Int a
    deriving Show

layout64 :: Tree a -> Tree (a, (Int, Int))
layout64 t = t'
    where
        (Weighted _ t') = layout 1 1 t

        layout :: Int -> Int -> Tree a -> Weighted (Tree (a, (Int, Int)))
        layout _ _ Empty = Weighted 0 Empty
        layout x y (Branch k l r) = Weighted (wl + 1 + wr) (Branch (k, (x + wl, y)) l' r')
            where
                (Weighted wl l') = layout x (y + 1) l
                (Weighted wr r') = layout (x + wl + 1) (y + 1) r

