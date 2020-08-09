import Data.List

--одночлен представляется в виде пары (x, [p_a,p_b,p_c..p_f])
-- x - коэффициент, p_i - показатель степени соответствующей переменной
--переменные A, B, C, D, E, F
--необходимо указать степени для всех переменных например:
--(2.0, [1.5,3.0,2.0,0,0,0]) = 2 * A^1.5 * B^3 * C*2

data Monom = Monom (Double, [Double])
--многочлен это сумма одночленов
data Polynom = Polynom [Monom]

--сложение одночленов
monosum :: Monom -> Monom -> Polynom
monosum a b = Polynom $ [a] ++ [b]
--monosum (Monom (3.0,[0,1.0,0,2.0,0,0])) (Monom (2.0,[0,0,4.0,5.0,0,0]))
-- => 3.0 * B^1.0 * D^2.0 + 2.0 * C^4.0 * D^5.0

--сложение многочленов
polysum :: Polynom -> Polynom -> Polynom
polysum (Polynom a) (Polynom b) = Polynom $ a ++ b 
-- polysum ( Polynom [ (Monom (3.0,[0,0,0,2.0,0,0])), (Monom (2.0,[0,0,4.0,0,0,0]))]) (Polynom [ (Monom (7.0,[0,1.0,0,0,0,0])), (Monom (2.8,[5.5,0,0,0,0,0]))])
-- => 3.0 * D^2.0 + 2.0 * C^4.0 + 7.0 * B^1.0 + 2.8 * A^5.5

--умножение одночленов
monomulmono :: Monom -> Monom -> Monom
monomulmono (Monom (x,l)) (Monom (y,m)) = Monom $ (x*y, listsum l m)
-- monomulmono (Monom (3.0,[0,1.0,0,2.0,0,0])) (Monom (2.0,[0,0,4.0,5.0,0,0]))
-- => 6.0 * B^1.0 * C^4.0 * D^7.0

--сокращение степеней
listsum :: [Double] -> [Double] -> [Double]
listsum [] [] = []
listsum l m = zipWith (+) l m

--умножение одночлена на многочлен
monomulpoly :: Monom -> Polynom -> Polynom
monomulpoly _ (Polynom []) = Polynom []
monomulpoly a (Polynom p) = Polynom $ (monomulmono a (head p)) : pek
  where
    kek = monomulpoly a (Polynom (tail p))
    pek = (\(Polynom p) -> p) kek

--умножение многочленов
concat_polynom :: [Polynom] -> Polynom
concat_polynom [] = Polynom []
concat_polynom ((Polynom x):xs) = Polynom $ x ++ pek
  where
    kek = concat_polynom xs
    pek = (\(Polynom p) -> p) kek

polymul :: Polynom -> Polynom -> Polynom
polymul p q = concat_polynom (polymul' p q)

polymul' :: Polynom -> Polynom -> [Polynom]
polymul' _ (Polynom []) = []
polymul' (Polynom []) _ = []
polymul' a@(Polynom p) b@(Polynom q) = (monomulpoly (head p) b) : (polymul' (Polynom (tail p)) b)



instance Show Monom where 
   show (Monom x) = if (fst x) == 0 then show (fst x) 
    else (show (fst x))
      ++ (if (snd x)!!0 /= 0 then " * A^" ++ show ((snd x)!!0)  else "") 
      ++ (if (snd x)!!1 /= 0 then " * B^" ++ show ((snd x)!!1)  else "") 
      ++ (if (snd x)!!2 /= 0 then " * C^" ++ show ((snd x)!!2)  else "") 
      ++ (if (snd x)!!3 /= 0 then " * D^" ++ show ((snd x)!!3)  else "") 
      ++ (if (snd x)!!4 /= 0 then " * E^" ++ show ((snd x)!!4)  else "") 
      ++ (if (snd x)!!5 /= 0 then " * F^" ++ show ((snd x)!!5)  else "") 


instance Show Polynom where
  show (Polynom p) = intercalate " + " $ map show p

--Polynom [ (Monom (3.0,[0,1.0,0,2.0,0,0])), (Monom (2.0,[0,0,4.0,5.0,0,0]))]
-- => 3.0 * B^1.0 * D^2.0 + 2.0 * C^4.0 * D^5.0