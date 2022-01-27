
-- node value and three branches on the side 
data TreeT a = Empty | Leaf a | Node a (TreeT a) (TreeT a) (TreeT a) deriving (Show, Eq)

instance Functor TreeT where
    fmap f Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node value left middle right) = Node (f value) (fmap f left) (fmap f middle) (fmap f right)

-- height
instance Foldable TreeT where
    foldr f z Empty = z
    foldr f z (Leaf a) = f a z
    foldr f z (Node val left mid right) = 
        let 
            rightFold = foldr f z right
            middleFold = foldr f rightFold mid
            in 
                foldr f (f val middleFold) left
            
maxim :: Ord a => a -> a -> a
maxim a b
    | a >= b = a
    | otherwise = b

treeHeight :: TreeT a -> Int 
treeHeight Empty = 0
treeHeight (Leaf a) = 1
treeHeight (Node val left mid right) = 1 + maxim (maxim (treeHeight left) (treeHeight mid)) (treeHeight right)

-- to test
b = Node 3 Empty (Node 4 Empty (Leaf 5) (Leaf 6)) (Leaf 3)
-- should be 3
test = treeHeight b