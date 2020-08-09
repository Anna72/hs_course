import Data.Ratio
--1.1
-- факториал
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- биномиальный коэффициент
cnk :: Integer -> Integer -> Integer
cnk n k =  (factorial n) `div` ( (factorial k) * (factorial (n - k)) )
-- числа каталана
catalan 0 = 1
catalan n = (cnk (2*n) n) `div` (n + 1)

--1.2
-- фильтруем пустые списки
notNull :: [a] -> Bool
notNull ls = not (null ls)

-- объединение списков
merge :: [[a]] -> [a]
merge [] = []
merge ls = merge' (filter notNull ls)
merge' :: [[a]] -> [a]
merge' ls = map head ls ++ merge (map tail ls)