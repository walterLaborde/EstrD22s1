

{-
En la heapSort que solo usa la PriorityQueue el costo es O(n) porque findMinPQ y deleteMinPQ tienen 
ese costo...
Si el usuario usa una Heap como estructura para esas operaciones el costo es O(log n) porque insertPq
tiene costo constante.
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