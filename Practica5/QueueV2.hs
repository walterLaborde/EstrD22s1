module QueueV2 (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)
    where

data Queue a = Q [a]
{-
Inv.Rep.: 
    los elementos se encolan por el final de la lista y se desencolan por delante
-}
-- emptyQ :: Queue a
-- Crea una cola vacía.
-- isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
-- queue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
-- dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.

emptyQ :: Queue a -- O(1)
emptyQ = Q []
-- Crea una cola vacía.

isEmptyQ :: Queue a -> Bool -- O(1)
isEmptyQ (Q xs) = null' xs

null' :: [a] -> Bool
null' [] = True
null' _ = False
-- Dada una cola indica si la cola está vacía.

queue :: a -> Queue a -> Queue a -- O(1) 
queue y (Q xs) = Q (y:xs)
-- Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a -- O(n) es lineal en "reversa"
firstQ (Q xs) = head' (reversa xs)

head' :: [a] -> a
head' (x:_) = x
head' _ = error "head no se puede usar con []"

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
-- Dada una cola devuelve el primer elemento de la cola.

dequeue :: Eq a => Queue a -> Queue a -- O(1) es lineal en la funcion "sinElElemento".
dequeue (Q xs) = Q (sinElElemento (last' xs) xs)

sinElElemento :: Eq a => a -> [a] -> [a]
sinElElemento y []     = []
sinElElemento y (x:xs) = singularSi x (not (y == x)) ++ sinElElemento y xs
-- Dada una cola la devuelve sin su primer elemento.

last' :: [a] -> a 
last' []     = error "no puede ser lista vacia"
last' (x:xs) = if esVacio xs
                then x
                else last' xs

esVacio :: [a] -> Bool
esVacio [] = True
esVacio _  = False

singularSi :: a -> Bool -> [a]
singularSi x True = [x] 
singularSi x _    = []

