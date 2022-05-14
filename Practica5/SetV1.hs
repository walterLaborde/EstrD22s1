module Setv1 (emptyS,addS,belongs,sizeS,removeS,unionS,setToList)
    where

data Set a = S [a] Int
--inv. rep: 
    -- el Set no tiene repetidos 
    -- Int representa a la cantidad de elementos que guarda el Set       

emptyS :: Set a
emptyS = S [] 0
-- Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
addS y (S xs m) = if pertenece y xs
                    then S xs m
                    else S (y:xs) (m+1)
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.

addS' :: Eq a => a -> Set a -> Set a 
addS' y (S xs m) = S (agregarSiNoEsta y xs) (sumarSiAgregue y xs m) 

agregarSiNoEsta :: Eq a => a -> [a] -> [a]
agregarSiNoEsta y xs = if pertenece y xs
                          then xs
                          else y : xs

sumarSiAgregue :: Eq a => a -> [a] -> Int -> Int
sumarSiAgregue y xs m = if pertenece y xs 
                          then m 
                          else m+1

belongs :: Eq a => a -> Set a -> Bool
belongs y (S xs m) = pertenece y xs
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int
sizeS (S _ m) = m
-- Devuelve la cantidad de elementos distintos de un conjunto.

removeS :: Eq a => a -> Set a -> Set a
removeS y (S xs m) = if pertenece y xs 
                        then S (sinElElemento y xs) (m-1)
                        else S xs m

sinElElemento :: Eq a => a -> [a] -> [a]
sinElElemento y []     = []
sinElElemento y (x:xs) = singularSi x (not (y == x)) ++ sinElElemento y xs
-- Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n) (S ys m) = S (xs ++ ys) (n+m) 
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.


setToList :: Eq a => Set a -> [a]
setToList (S xs _) = [xs]

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
