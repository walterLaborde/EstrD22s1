module StackV1 (Stack,emptyS,isEmptyS,push,top,pop,lenS)
    where

data Stack a = Stc [a] 
{-Inv.Rep:
    * No encuentro -}

emptyS :: Stack a -- O(1)
emptyS = Stc []
-- Crea una pila vacía.

isEmptyS :: Stack a -> Bool -- O(1)
isEmptyS (Stc xs) = null' xs
-- Dada una pila indica si está vacía.

null' :: [a] -> Bool
null' [] = True
null' _ = False

push :: a -> Stack a -> Stack a -- O(1)
push y (Stc xs) = Stc (y:xs)
-- Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a -- O(1)
top (Stc xs) = head' xs
--Dada un pila devuelve el elemento del tope de la pila.

head' :: [a] -> a
head' (x:xs) = x

pop :: Stack a -> Stack a -- O(1)
pop (Stc xs) = Stc (tail' xs)

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' _      = error "la lista no puede ser vacia"
-- Dada una pila devuelve la pila sin el primer elemento.

lenS :: Stack a -> Int -- O(n) por la funcion "longitud"
lenS (Stc xs) = longitud xs

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns
-- Dada una pila devuelve la pila sin el primer elemento. //// ESTA MAL... DEBE SER length
-- Costo: constante.

