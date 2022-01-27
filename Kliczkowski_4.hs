
delete :: Eq a => a -> [a] -> [a]
delete x xs = [y|y<-xs, y/=x]

remove_duplicates :: Eq a => [a] -> [a]
remove_duplicates xs = rd xs [] where
  rd [] acc = acc
  rd (x:xs) acc = rd (delete x xs) (x:acc)

-- moving guy
move :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
move (n,k) (x,y) = [(x+i, y+j) | i <- [3,-3], j <- [1, -1], abs(x+i) < n, (x+i)>= 0, (y+j) >= 0, abs (y+j) < k]
   ++ [(x+i, y+j) | i <- [1,3, -1, -3], j<-[2, -2], abs(x+i) < n, abs (y+j) < k, (x+i)>= 0, (y+j) >= 0]

move_many :: Int -> (Int,Int) -> (Int, Int) -> [(Int,Int)]
move_many 0 size start = [start]
move_many num size start =  remove_duplicates ((move size start) >>= move_many (num - 1) size)


checkIfPossible :: Int -> (Int,Int) -> (Int, Int) -> Bool
checkIfPossible 0 _ _ = False 
checkIfPossible _ (0,0) _ = False 
checkIfPossible num (n,k) (x1, x2)
    | x1 < 0 || x2 < 0 = False 
    | otherwise = tmp (move_many num (n,k) (x1,x2))
        where
        tmp ls 
            | length ls  == n * k = True
            | otherwise = False