module Lib
    ( someFunc,
      ifElse,
      -- ex 1
      squareSum,
      -- ex 4
      myCurry,
      myUncurry
    ) where

import System.TimeIt
import qualified Data.Set as Set

checkPrime :: Integer -> Bool
checkPrime primeCandidate = g 2 primeCandidate (floor (sqrt (fromIntegral primeCandidate))) where
   g x number y
     | x >= y + 1 = True
     | number `mod` x == 0 = False
     | otherwise = g (x+1) number y



ifElse :: Bool -> (a, a) -> a
ifElse cond (onTrue, onFalse)
  | cond = onTrue
  | otherwise = onFalse

-- Ex 1 -------------------------------------------------------------------------
squareSum :: [Integer] -> Integer
squareSum [] = 0
squareSum x = foldl (+) 0 (map (^2) x)

-- Ex 2 -------------------------------------------------------------------------
squareSumPrime :: [Integer] -> Integer
squareSumPrime [] = 0
squareSumPrime x = foldl (+) 0 [tmp^2 | tmp <- x, checkPrime tmp ]
-- Ex 3 -------------------------------------------------------------------------
modulo2 :: Integer  -> Integer -> Integer
modulo2 x y = x + (y + 1) `mod` 2

countEven :: [Integer] ->Integer
countEven [] = 0
countEven x = foldl modulo2 0 x



  -- ex 4 -------------------------------------------------------------------------
addWithOne :: (Num a, Num b) => (a, b) -> a -> (a, b)
addWithOne (x,c) y = (x+y, c+1) 

countAv :: (Fractional a, Foldable t) => t a -> a
countAv x = sm / ln where
  (sm, ln) = foldl addWithOne (0,0) x
  
  
  
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f a = f (fst a) (snd a)
  -- ex 5 -------------------------------------------------------------------------

mymap :: (a -> b) -> [a] -> [b]
mymap pred ls = foldr tmp [] ls where
  tmp x y = pred x : y
  
 
  -- ex 6 -------------------------------------------------------------------------

adRev :: [a] -> a -> [a]
adRev a b = b:a

revRev :: [[a]] -> [[a]]
revRev = let rev a elem = foldl adRev [] elem : a 
         in foldl rev []


  -- ex 7 -------------------------------------------------------------------------
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter pred ls = foldr f [] ls where
  f a b
    | pred a = a : b
    | otherwise = b
 
my_filter2 :: (a -> Bool) -> [a] -> [a]
my_filter2 pred [] = []
my_filter2 pred (xs:ls)
  | pred xs = xs : my_filter2 pred ls
  | otherwise = my_filter2 pred ls

  -- ex 8 -------------------------------------------------------------------------
approxE :: Int -> Double 
approxE n = sum (foldl tmp [1.0] [1..n]) where
  tmp :: [Double] -> Int -> [Double]
  tmp (as:a) b = (as / fromIntegral b) : (as:a)


-- MAIN -----------------------------------------------------------------------
someFunc :: IO ()
someFunc = do
  print "This is a library for excercises from lab 1"
  -- ex 1 -------------------------------------------------------------------------
  --putStrLn "ex1 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (squareSum [1..3])
  -- ex 2 -------------------------------------------------------------------------
  --putStrLn "ex2 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (squareSumPrime [1..5])
  -- ex 3 -------------------------------------------------------------------------
  putStrLn "ex3 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (countEven a)
  -- ex 4 -------------------------------------------------------------------------
  putStrLn "ex4 : UNCURRY --------------------------------------------------------------------------------------------------------"
  print(countAv [1,2])
  
  --print(uncurry div (n, a3))
  putStrLn "ex4 : CURRY"
  --print(myCurry fst n a3)
  -- ex 5 -------------------------------------------------------------------------
  putStrLn "ex5 : MAPKA --------------------------------------------------------------------------------------------------------"
  print(mymap (^2) arrr)
  -- ex 6 -------------------------------------------------------------------------
  --putStrLn "ex6 : Euler --------------------------------------------------------------------------------------------------------"
  print(revRev strList)
  -- ex 7 -------------------------------------------------------------------------
  putStrLn "ex7 : Fibonacci naive -------------------------------------------------------------------------------------------------"
  timeIt $ print(my_filter2 (>0) [1,-1,4,7,9,-12,-10,0])
  putStrLn "Other way"
  timeIt $ print(my_filter (>0) [1,-1,4,7,9,-12,-10,0])
  -- ex 8 -------------------------------------------------------------------------
  putStrLn "ex8 : Fibonacci ZIPWITH -------------------------------------------------------------------------------------------------"
  timeIt $ print(approxE 10)
  where n = 31
        a = [1..23]
        a3 = 18
        n2 = 35 :: Int
        arrr = [1,1,2,3,3,5,5,3,3,3]
        strList = ["abc", "xyz"]
        str = "abcd"
        grr = [1,2,3]