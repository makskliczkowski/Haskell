fun :: (a->Bool) -> Int -> [a] -> [a]
fun _ 0 ls = ls
fun _ _ [] = []
fun p n (xs:x)
    | p xs = fun p (n-1) x
    | otherwise = xs : fun p n x


