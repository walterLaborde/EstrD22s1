import PriorityQueue

{-
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¾Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
práctica.
-}

heapSort :: Ord a => [a] -> [a]
heapSort xs = heapSort' (insertarXAPQ xs)

insertarXAPQ :: Ord a => [a] -> PriorityQueue a
insertarXAPQ []     = emptyPQ
insertarXAPQ (x:xs) = insertPq x (insertarXAPQ xs)

heapSort' :: PriorityQueue a -> [a]
heapSort' pq = if(isEmptyPQ pq) 
                  then []
                  else findMinPQ pq : deleteMinPQ pq

