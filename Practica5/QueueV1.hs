module QueueV1 (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)
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
queue y (Q xs) = Q (agregarAlFinal xs y)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e
-- Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a -- O(1)
firstQ (Q xs) = head' xs

head' :: [a] -> a
head' (x:_) = x
head' _ = error "head no se puede usar con []"
-- Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a -- O(1)
dequeue (Q xs) = Q (tail' xs)

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' _      = error "tail no se puede usar con []"
-- Dada una cola la devuelve sin su primer elemento.