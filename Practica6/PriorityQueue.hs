module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

    where
data PriorityQueue a = PQ [a]
{-
Inv.Rep:
    * no veo.
-} 

{-
emptyPQ :: PriorityQueue a
Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool
Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
Propósito: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a
Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía.
-}


emptyPQ :: PriorityQueue a -- O(1)
emptyPQ = PQ []
-- Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
isEmptyPQ (PQ xs) = null xs
-- Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a -- O(1)
insertPQ y (PQ xs) = PQ (y:xs)
-- Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a -- O(n)
findMinPQ (PQ xs) = minimum xs 
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a -- O(n)
deleteMinPQ (PQ xs) = PQ (borrarMin xs)

borrarMin :: Ord a => [a] -> [a] -- O(n)
-- Precond: la lista no puede ser vacia.
borrarMin xs = borrar (minimum xs) xs

borrar :: Eq a => a -> [a] -> [a] -- O(n)
borrar y []     = []
borrar y (x:xs) = if y==x then xs else x : borrar y xs
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.