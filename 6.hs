data Expr =
 Integral Integer | -- целые константы
 Function String | -- идентификаторы примитивных функций
 Variable String | -- переменная
 Lambda String Expr | -- лямбда-выражение
 Application Expr Expr -- применение функции

--То есть в нём нет boolean-ов и блоков (let, letrec). Задание - написать функцию, которая по заданному выражению выдаёт список имён всех встроенных (константных) функций, встречающихся в нём.

constFunc :: Expr -> [String]
constFunc (Function f) = [f]
constFunc (Lambda _ e) = eraseDupl (constFunc e)
constFunc (Application e1 e2) = eraseDupl (constFunc e1) ++ (constFunc e2)
constFunc _ = []

--удаление повторяющихся узлов списка
eraseDupl :: Eq a => [a] -> [a]
eraseDupl [] = []
eraseDupl (x:xs) 
    | x `elem` xs = eraseDupl xs
    | otherwise = x : eraseDupl xs
