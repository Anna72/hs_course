import Data.Function

class MyModulus a where
    modl :: a -> Int

instance MyModulus Int where
    modl = abs

instance MyModulus [a] where
    modl = length

instance MyModulus (a, b) where
    modl = const 2

data IntPara = Point Int Int

instance MyModulus IntPara where
    modl (Point x y) = on (+) modl x y


--  modl (5 :: Int)
-- => 5
--   modl [5, 6, 7]
-- => 3
--   modl (8, 9)
-- => 2
--   modl (Point 8 8)
-- => 16

import Data.Char

-- first task
-- partsum искомый ряд частичных сумм
factorial :: Double -> Double
factorial 0 = 1.0
factorial x = x*factorial(x-1)

taylorseries :: [Double]
taylorseries = [ 1.0 / (factorial i) | i <- [0, 1 .. ] ]

partsum :: [Double]
partsum = [ (sum ( take i $ taylorseries)) | i <- [1, 2 .. ] ] 

-- second task
dup :: [a] -> [a]
dup [] = []
dup (x : xs) = x : x : dup xs

-- third task
data Board = Board [[Bool]]

booltoPic :: Bool -> Char
booltoPic False = '░'
booltoPic True  = '█'

instance Show Board where
    show (Board x) = unlines $ map (map (booltoPic)) x

--Board [[True, False, True, False], [False, True, False, True], [True, False, True, True], [False, True, True, True]]