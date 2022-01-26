-- HERE WE WILL WRITE THINGS ABOUT HASKELL THAT CAN BE OF MUCH USE

-- -------------------------------------------------------- tail recursiveness
fact n = tmp n n where
    tmp _ 0 = 1
    tmp acc 1 = acc
    tmp acc a = tmp (acc * (a-1)) (a-1)

-- -------------------------------------------------------- some stuff about lists and reverses
rev :: [a] -> [a]
rev n = tmp n [] where
    tmp [] acc = acc
    tmp (x:xs) acc = tmp xs (x:acc)

-- -------------------------------------------------------- currying functions
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f (a,b) = f a b

-- -------------------------------------------------------- some pattern matching
tmp :: Int -> Int -> Int -> Int
tmp x y a
    | x > y = a + y
    | x < y = a + x
    | otherwise = a

-- -------------------------------------------------------- some stuff with list comprehension
tmpL lst = [(x,y) | x <- lst, y <- lst, x `mod` y == 3]

-- -------------------------------------------------------- using some fold left
squareSum [] = 0
squareSum x = foldl (+) 0 (map (^2) x)

-- foldleft starting from acc we do something with elements from the list from the first one, the acc is on the left side
-- foldright starting from acc on the right side we do something with elements on the right foldr 'f' 0 [1,2,3] = 1 'f' (foldr 0 [2,3]) = ...  1 'f' (2 'f' (3 'f' 0))   

-- map
mymap :: (a -> b) -> [a] -> [b]
mymap pred = foldr tmp [] where
  tmp x y = pred x : y

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr f [] where
  f a b
    | pred a = a : b
    | otherwise = b
 
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 pred [] = []
myfilter2 pred (xs:ls)
  | pred xs = xs : myFilter2 pred ls
  | otherwise = myFilter2 pred ls

-- -------------------------------------------------------- lambdas
add = \ x y -> x + y
pi1 = \ x y -> x
pi2 = \ x y -> y


-- składanie w Haskellu
-- skladanie (.) 
-- powinno być to samo
s = \g f x -> g (f x)
g x = x ^ 2 
f x = - x

-- złożenia moga być dużo większe
h12 = pi1.pi2
h21 = pi2.pi1
-- pi1.pi2 == (\g f x -> g (f x)) pi1 pi1 == {beta reduction} == \x -> pi1 (pi2 x) == \x -> pi1 ((\z y -> y) x) == {beta redukcja} == \x -> pi1 (\y -> y) == \x -> (\x y -> x) (\y -> y) == 
-- \x -> \y -> \z -> z== \x y z -> z

-- pi2.pi1 == (\g f x -> g (f x) ) p2 p1 == \x -> p2 ((\z y -> z) x) == \x -> p2 (\z -> z) == \x -> (\x y -> y) \z ->z == \x -> \y -> y

-- -------------------------------------------------------- typy
-- like constructor
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

-- fmap dla typów
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left middle right) = Node (fmap f left) (f middle) (fmap f right)
