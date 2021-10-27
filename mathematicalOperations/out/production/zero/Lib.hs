module Lib
    ( someFunc,
      ifElse,
      -- ex 1
      naiveFactorial,
      betterFactorial,
      divideFactorial,
      productFactorial,
      -- ex 2
      myReverse,
      naiveReverse,
      -- ex 3
      naiveLeadingZerosNFact,
      leadingZerosNFact,
      -- ex 4
      myCurry,
      myUncurry
    ) where

import System.TimeIt
import qualified Data.Set as Set

ifElse :: Bool -> (a, a) -> a
ifElse cond (onTrue, onFalse)
  | cond = onTrue
  | otherwise = onFalse

-- Ex 1 -------------------------------------------------------------------------

naiveFactorial :: Integer  -> Integer
naiveFactorial 0 = 1
naiveFactorial n = n * naiveFactorial (n - 1)

betterFactorial :: Integer  -> Integer
betterFactorial n = tmp n n where
  tmp _ 0 = 1
  tmp acc 1 = acc
  tmp acc a = tmp (acc * (a - 1)) (a - 1)

productFactorial :: Integer -> Integer
productFactorial n = product [1..n]

divideFactorial :: Integer -> Integer
divideFactorial n = tmp 1 n where
  tmp :: Integer -> Integer -> Integer
  tmp x y
    | x > y = 1
    | x == y = x
    | otherwise = let c = div (x+y) 2 in tmp x c * tmp (c+1) y

-- Ex 2 -------------------------------------------------------------------------
naiveReverse :: [a] -> [a]
naiveReverse [] = []
naiveReverse (x:xs) = naiveReverse xs ++ [x]

myReverse :: [a] -> [a]
myReverse n = tmp n [] where
  tmp [] acc = acc
  tmp (x:xs) acc = tmp xs (x:acc)

-- Ex 3 -------------------------------------------------------------------------
naiveLeadingZerosNFact :: Integer -> Integer
naiveLeadingZerosNFact n = g (divideFactorial n) 0 where
  g :: Integer -> Integer -> Integer
  g num acc
      | num `mod` 10 /= 0 = acc
      | otherwise = g (div num 10) (acc + 1)


leadingZerosNFact :: Integer -> Integer
leadingZerosNFact n = tmp n 5 0 where
  tmp :: Integer -> Integer -> Integer -> Integer
  tmp a power acc
    | a < power = acc
    | otherwise = tmp a (power * power) (acc + div a power)
  -- ex 4 -------------------------------------------------------------------------
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f a = f (fst a) (snd a)
  -- ex 5 -------------------------------------------------------------------------
checkPrime :: Integer -> Bool
checkPrime primeCandidate = g 2 primeCandidate (floor (sqrt (fromIntegral primeCandidate))) where
   g x number y
     | x >= y + 1 = True
     | number `mod` x == 0 = False
     | otherwise = g (x+1) number y

markEveryNth :: Integer -> [Bool] -> [Bool]
markEveryNth n tab = tmp n tab [] where
    tmp :: Integer -> [Bool] -> [Bool] -> [Bool]
    tmp which table acc
        | null table = acc
        | which == 1 = tmp n (tail table) (False:acc)
        | otherwise = tmp (which + 1) (tail table) (head table:acc)

sieve :: Int -> [Integer]
sieve n = tmp (take (n-2) ones) [] 2 where
  tmp [] acc _ = acc
  tmp (x:tab) acc counter
      | not x = tmp tab acc (counter + 1)                                        -- don't take x
      | checkPrime counter = tmp tab (counter:acc) (counter + 1)                 -- check if it's prime, then add
      | otherwise = tmp (reverse (markEveryNth counter tab)) acc (counter + 1)   --
  ones = True : ones :: [Bool]
  -- ex 6 -------------------------------------------------------------------------

eulerTotient :: Integral a => a -> Int
eulerTotient n = length ([x | x <- [1..n], gcd x n == 1])

eulerTotientWithD :: Integral a => a -> a -> [a]
eulerTotientWithD n d = [x | x <- [1..n], gcd x n == d]

eulerTotientPrint :: (Show a, Integral a) => a -> IO ()
eulerTotientPrint n = print [x | x <- [1..n-1], gcd x n == 1]

naiveEulerSum :: Integral a => a -> Int
naiveEulerSum n = sum [eulerTotient x | x <- [1..n], n `mod` x == 0]

testEulerSum :: Integral a => a -> Int
testEulerSum n = sum [length (eulerTotientWithD n d) | d <- [y | y <- [1..n], n `rem` y == 0]]
  -- ex 7 -------------------------------------------------------------------------

naiveFibonacci :: Integer -> Integer
naiveFibonacci 0 = 1
naiveFibonacci 1 = 1
naiveFibonacci n = naiveFibonacci (n-1) + naiveFibonacci (n-2)

dynamicFibonacci :: (Num a, Eq t, Num t) => t -> (a, [a])
dynamicFibonacci n = tmp 1 1 2 [1,1] where
  tmp fkm2 fkm1 k acc
    | k == n = (fkm2 + fkm1, (fkm2 + fkm1) : acc)
    | otherwise = tmp  fkm1 (fkm1 + fkm2) (k + 1) ((fkm1 + fkm2) : acc)

dynamicFibonacciWithoutList :: (Num t1, Eq t2, Num t2) => t2 -> t1
dynamicFibonacciWithoutList n = tmp 1 1 2 where
  tmp fkm2 fkm1 k
    | k == n = fkm2 + fkm1
    | otherwise = tmp  fkm1 (fkm1 + fkm2) (k + 1)

dynamicOtherguy :: Integer -> (Integer, [Integer])
dynamicOtherguy n = tmp 1 1 2 [1,1] where
  tmp fkm2 fkm1 k acc
    | k == n = (fkm2 + fkm1 + c, (fkm2 + fkm1 + c) : acc)
    | otherwise = tmp  fkm1 (fkm1 + fkm2) (k + 1) ((fkm1 + fkm2 + c) : acc)
    where
      c :: Integer
      c = (div (k*(k+1)) 2) - 1
  -- ex 8 -------------------------------------------------------------------------
zipFibonacci :: [Integer]
zipFibonacci = 1 : 1 : zipWith (+) zipFibonacci (tail zipFibonacci)
  -- ex 9 -------------------------------------------------------------------------
ecd :: Eq a => [a] -> [a]
ecd a = reverse (tmp (tail a) (head a) [head a]) where
    tmp [] _ acc = acc
    tmp (xs:arr) x acc
      | xs == x = tmp arr x acc
      | otherwise = tmp arr xs (xs:acc)

encode :: (Eq a, Num b) => [a] -> [(a, b)]
encode a = reverse (tmp (tail a) (head a) (head a, 1) []) where
    tmp [] _ a acc = a:acc
    tmp (xs:arr) x (_, occurences) acc
      | xs == x = tmp arr x (x, occurences + 1) acc
      | otherwise = tmp arr xs (xs, 1) ((x,occurences):acc)


decode :: (Eq b, Num b) => [(a, b)] -> [a]
decode a = reverse(tmp (head a) (tail a) []) where
  tmp (_,0) [] acc = acc
  tmp (_,0) (xs:arr) acc = tmp xs arr acc
  tmp (x, num) arr acc = tmp (x, num - 1) arr (x:acc)
  -- ex 10 -------------------------------------------------------------------------
revrev :: [[a]] -> [[a]]
revrev a = tmp (head a) (tail a) [] where
  tmp x [] acc = (reverse x):acc
  tmp x (xs:arr) acc = tmp xs arr ((reverse x) : acc)
  -- ex 11 -------------------------------------------------------------------------
substrings :: [a] -> [[a]]
substrings a = tmp a (reverse a) [] where
  tmp [] [] acc = acc
  tmp [] (_:arr) acc = tmp (reverse arr) arr acc
  tmp (x1:arr1) (x2:arr2) acc = tmp arr1 (x2:arr2) ((x1:arr1):acc)


  -- ex 12 -------------------------------------------------------------------------
productList a b = [[x] ++ [y] | x <- a, y <- b]
  

  -- ex 13 -------------------------------------------------------------------------
rotateLeft :: [a] -> [a]
rotateLeft (x:arr) = arr ++ [x]

permu :: [a] -> [[a]]
permu a = tmp [] a (length a) where
      tmp leftAcc [] _ = [leftAcc]
      tmp leftAcc (x:arr) rotations
        | rotations == 1 = tmp (leftAcc ++ [x]) arr (length arr)
        | otherwise = tmp leftAcc g (rotations - 1) ++ tmp (leftAcc ++ [x]) arr (length arr)
          where g = rotateLeft (x:arr)


-- MAIN -----------------------------------------------------------------------
someFunc :: IO ()
someFunc = do
  print "This is a library for excercises from lab 1"
  -- ex 1 -------------------------------------------------------------------------
  putStrLn "ex1 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (naiveFactorial n)
  putStrLn "ex1 : WITHOUT STACK"
  timeIt $ print (betterFactorial n)
  putStrLn "ex1 : WITH PRODUCT"
  timeIt $ print (productFactorial n)
  putStrLn "ex1 : WITH DIVIDE"
  timeIt $ print (divideFactorial n)
  -- ex 2 -------------------------------------------------------------------------
  putStrLn "ex2 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (naiveReverse a)
  putStrLn "ex2 : WITHOUT STACK"
  timeIt $ print (myReverse a)
  -- ex 3 -------------------------------------------------------------------------
  putStrLn "ex3 : NAIVE --------------------------------------------------------------------------------------------------------"
  timeIt $ print (naiveLeadingZerosNFact a3)
  putStrLn "ex3 : QUICKER - without factorial computation"
  timeIt $ print (leadingZerosNFact a3)
  -- ex 4 -------------------------------------------------------------------------
  putStrLn "ex4 : UNCURRY --------------------------------------------------------------------------------------------------------"
  print(uncurry div (n, a3))
  putStrLn "ex4 : CURRY"
  print(myCurry fst n a3)
  -- ex 5 -------------------------------------------------------------------------
  putStrLn "ex5 : Sieve --------------------------------------------------------------------------------------------------------"
  print(sieve 33)
  -- ex 6 -------------------------------------------------------------------------
  putStrLn "ex6 : Euler --------------------------------------------------------------------------------------------------------"
  print(eulerTotient a3)
  eulerTotientPrint a3
  putStrLn "testing sum - standard"
  print(naiveEulerSum a3)
  putStrLn "testing sum - with proof"
  print(testEulerSum a3)
  -- ex 7 -------------------------------------------------------------------------
  putStrLn "ex7 : Fibonacci naive -------------------------------------------------------------------------------------------------"
  timeIt $ print(naiveFibonacci n)
  putStrLn "Other way"
  timeIt $ print(dynamicFibonacci n)
  putStrLn "This other thingy"
  timeIt $ print(dynamicOtherguy 3)
  -- ex 8 -------------------------------------------------------------------------
  putStrLn "ex8 : Fibonacci ZIPWITH -------------------------------------------------------------------------------------------------"
  timeIt $ print(take (n2 + 1) zipFibonacci)

  -- ex 9 -------------------------------------------------------------------------
  putStrLn "ex9 : SOME STUFF -------------------------------------------------------------------------------------------------"
  print(arrr)
  print(ecd arrr)
  print(arr)
  print(decode arr)
  -- ex 10 -------------------------------------------------------------------------
  putStrLn "ex10 : MORE STUFF -------------------------------------------------------------------------------------------------"
  print (revrev strList)
  -- ex 11 -------------------------------------------------------------------------
  putStrLn "ex11 : AND OTHER STUFF -------------------------------------------------------------------------------------------------"
  print(substrings str)
  -- ex 12 -------------------------------------------------------------------------
  putStrLn "ex12 : GRRR BASICALY THE SAME -------------------------------------------------------------------------------------------------"
  print(productList [[]] grr)
  -- ex 13 -------------------------------------------------------------------------
  putStrLn "ex13 : SOME PERM -------------------------------------------------------------------------------------------------"
  print(rotateLeft grr)
  print(permu str)


  where n = 31
        a = [1..23]
        a3 = 18
        n2 = 35 :: Int
        arrr = [1,1,2,3,3,5,5,3,3,3]
        arr = encode arrr
        strList = ["abc", "xyz"]
        str = "abcd"
        grr = [1,2,3]