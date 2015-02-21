data Tree a = Empty | Node a (Tree a) (Tree a)

tree :: Tree Int
tree = (Node 1 
        (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty))
        (Node 3 (Node 6 Empty Empty) Empty))

-- inorder, preorder, postorder
flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node n leftTree rightTree) = flattenTree leftTree ++ [n] ++ flattenTree rightTree

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ leftTree rightTree) = 1 + max (treeHeight leftTree) (treeHeight rightTree)

instance Show a => Show (Tree a) where
    show tree = show $ flattenTree tree

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ leftTree rightTree) = 1 + countNodes leftTree + countNodes rightTree

treeLevel :: Int -> Tree a -> [a]
treeLevel level tree = helper 1 level tree
    where
        helper _ _ Empty = []
        helper currentLevel desiredLevel (Node x leftTree rightTree)
            | currentLevel == desiredLevel = [x]
            | otherwise = helper (currentLevel + 1) desiredLevel leftTree ++ helper (currentLevel + 1) desiredLevel rightTree

--treeLevels :: Tree a => [a]
treeLevels tree = [treeLevel x tree | x <- [1..treeHeight tree]]

levelSum :: Int -> Tree Int -> Int
levelSum level tree = sum $ treeLevel level tree

