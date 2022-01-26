-- HERE WE WILL WRITE THINGS ABOUT HASKELL THAT CAN BE OF MUCH USE
import Data.Set
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

-- Foldable
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node left a right) = treeFoldr f (f a (treeFoldr f z right)) left
  
  foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f z Empty = z
  foldl f z (Leaf b) = f z b
  foldl f z (Node left b right) = treeFoldl f (f (treeFoldl f z left) b) right


  -- counts
  countNodes :: Tree a -> Int
  countNodes tr = treeFoldr (+) 0 $ treeMap (\x -> 1) tr
-- -------------------------------------------------------- funktory i inee stwory
-- join :: (Monad m) => mma -> ma (to jest nasze mu)
-- for lists -> join [list1, ... , listn]
f x = [x+1, x+2]
g x = [2*x, 3*x]

-- bind w notacji do
-- return tworzy typ monadyczny
f = do x<-[1..6]; y<-[1..20]; return (x+y)
-- jak rozpisac do ?
-- [1..6] >>= \x-> do (y<-[1..20]; return (x+y)) = [1..6] >>= (\x -> [1..20] >>= \y -> return (x+y)) = {return (x+y) = [x+y]} = [1..6] >>= (\x -> join [[x+1], [x+2], ... [x+20]]) =
-- join [[1+1, 1+2, 1+3,..., 1+ 20], [2+1, 2+2, ..., 2 + 20]...]

-- EX 3
-- do x <- mx; f x = mx >>= \x -> (f x) = mx >>= f

-- Zad 8
move :: Int -> Int -> Maybe Int
move step pos
  |  abs (pos + step) > 2 || abs (pos) > 2 = Nothing
  | otherwise = Just (step + pos)

move_list :: [Int] -> Int -> Maybe Int
move_list [] x = (Just x)
move_list (xs:x) n = (move xs n) >>= (move_list x)

remove_dups = head.group.sort
remove_dups2 = toList.fromList
-- Zad 9
move_knight :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
move_knight (n,k) (x,y) = map remove_dups [(x+i, y+j) | i <- [1,-1], j <- [2,-2], abs(x+i) < n, (x+i)> 0, (y+j) > 0, abs (y+j) < k]
   ++ [(x+j, y+i) | i <- [1,-1], j<- [2,-2], abs(x+j) < n, abs (y+i) < k, (x+j)> 0, (y+i) > 0]

move_knight_many 0 size start = [start]
move_knight_many num size start = remove_dups ((move_knight size start) >>= move_knight_many (num - 1) size)


data Vector3 a = Vector3 a a a 
instance Show a => Show (Vector3 a) where
  show (Vector3 x1 x2 x3) = "V = [" ++ show x1 ++ ", " ++ show x2 ++ ", " ++ show x3  ++ "]"