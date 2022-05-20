-- ///////////////////////////
-- Queue con 2 listas        /
-- ///////////////////////////

{-
Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).

La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero
todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
vacía, entonces la cola se encuentra vacía.
-}

-- module Queue2L (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)
--     where

data Queue a = Q [a] [a]
{-
Inv.Rep.: 
    la estructura tiene 2 listas, una "fs" por la que se quitan elementos y otra "bs" por la que se 
    agregan. si fs esta vacia, la cola esta vacia.
-}

emptyQ :: Queue a -- O(1)
emptyQ = Q [] []
-- Crea una cola vacía.

isEmptyQ :: Queue a -> Bool -- O(1)
isEmptyQ (Q [] _) = True
isEmptyQ _        = False
-- Dada una cola indica si la cola está vacía.

queue :: a -> Queue a -> Queue a -- O(1)
queue z (Q xs ys) = if null' xs
                      then Q (z:xs) ys
                      else Q xs (z:ys)
-- Dados un elemento y una cola, agrega ese elemento a la cola.

null' :: [a] -> Bool
null' [] = True
null' _ = False

firstQ :: Queue a -> a --  O(1)
firstQ (Q xs ys) = if null' xs 
                      then head' ys
                      else head' xs
-- Dada una cola devuelve el primer elemento de la cola.

head' :: [a] -> a
head' []     = error "la lista no puede ser vacia"
head' (x:xs) = x

dequeue :: Queue a -> Queue a -- O()
dequeue (Q [] ys) = Q (tail' ys) []
dequeue (Q xs ys) = if esListaUnitaria xs
                      then Q ys []
                      else Q (tail' xs) ys
-- Dada una cola la devuelve sin su primer elemento.

esListaUnitaria :: [a] -> Bool
esListaUnitaria (x:[]) = True
esListaUnitaria _      = False

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' _      = error "la lista no puede ser vacia"

