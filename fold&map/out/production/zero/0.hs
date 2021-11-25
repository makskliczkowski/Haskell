module BMI
 (
 sig,
 tellBmi,
 ad,

 )where

sig :: Double -> Int
sig x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

tellBmi :: Double -> Double -> String
tellBmi h w =
 let call_bmi x y = y / ( x ^ 2)
     g c
       | c <= 18.5 = "you are underweight"
       | (c < 25) = "you have normal weight"
       | otherwise = "you are overweight"
 in g (call_bmi h w)


ad :: Int -> Int -> Int
ad x y = x + y
