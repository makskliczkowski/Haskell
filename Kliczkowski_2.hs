f x y = y - x
g x = 3 ^ x

-- f.g = (\a b x -> a (b x) ) f g = {beta reduction} = \x -> f (g x) = \x -> f (3 ^ x) = \x -> ((\y x -> y - 3^x) ) = \x y -> y - 3^x
-- :t f g == f.g :: (Integral a, Num b) => a -> b -> b

check1 = (f.g) 1 2
check2 = (f.g) 2 1

check3 = (f.g) 3 1
