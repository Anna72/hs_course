import Data.Ratio
-- факториал
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- n-ая цифра числа e
nDigitFromE :: Integer -> Integer
nDigitFromE n =  (floor ((sum [ 1 % (factorial k) | k <- [0..(n+10)]]) * 10^n)) `mod` 10

-- бесконечный список знаков после запятой числа е
infiniteE = map nDigitFromE [1..]
-- число е = 2,7182818284 5904523536 0287471352 6624977572 4709369995 9574966967 6277240766 3035354759 4571382178 5251664274...
--результат выполнения вызова take 100  infiniteE 
-- => [7,1,8,2,8,1,8,2,8,4,5,9,0,4,5,2,3,5,3,6,0,2,8,7,4,7,1,3,5,2,6,6,2,4,9,7,7,5,7,2,4,7,0,9,3,6,9,9,9,5,9,5,7,4,9,6,6,9,6,7,6,2,7,7,2,4,0,7,6,6,3,0,3,5,3,5,4,7,5,9,4,5,7,1,3,8,2,1,7,8,5,2,5,1,6,6,4,2,7,4]


import Data.Char


-- 

data Board = Board [[Bool]]

booltoPic :: Bool -> Char

--booltoPic False = '░'
--booltoPic True  = '█' 

booltoPic False = '+'
booltoPic True  = '-' 

instance Show Board where
    show (Board x) = unlines $ map (map (booltoPic)) x

--Board [[True, False, True, False], [False, True, False, True], [True, False, True, True], [False, True, True, True]]