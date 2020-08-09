import Control.Monad

type Vertex = (Int,[Int])
type Graph = [Vertex]

-- в графе вершины нумеруются с 1 , записаны попорядку , например [(1, [2,3]), (2, [1,3]), (3, [1])]
--2.1
-- функция для графа и одной конкретной вершины проверяет есть ли на каждое исходящее из нее ребро соответсвующее входящее
isCorrect :: Graph -> Vertex -> Bool -> Bool
isCorrect [] v b = b
isCorrect graph (v,[]) b = b
isCorrect graph (i,(v:s)) b = 
    (&&) (elem i (snd (graph !! (v-1)))) (isCorrect graph (i,s) b)

-- проверка неориентированности графа
isCorrectGraph :: Graph -> Bool
isCorrectGraph graph = all (\v -> isCorrect graph v True) graph

--2.2
-- смежные вершины
adjacentV :: Graph -> Int -> [Int]
adjacentV graph v = case (lookup v graph) of 
    Just s -> s
    Nothing -> []

-- поиск в ширину
bfs :: [Int] -> Graph -> Int -> [Int]
bfs used graph u = u : (adjacentV graph u >>= \v -> guard (notElem v used) >> bfs (u : used) graph v)

-- проверка, состоит ли граф из одной компоненты связности
isConnected :: Graph -> Bool
isConnected g@((node, _) : _) = null $ filter (flip notElem (bfs [] g node)) (map fst g)
