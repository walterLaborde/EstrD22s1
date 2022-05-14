SetV2

module SetV2 (emptyS,addS,belongs,sizeS,removeS,unionS,setToList)
    where

data Set a = S [a]
--inv. rep: 
    -- el Set admite repetidos

-- //////// EFICIENCIA vs SetV1
{-
la eficiencia es mejor en SetV1 porque, como en SetV2 recibo una lista que puedo tener repetidos pero 
el usuario debe seguir viéndolo como un conjunto, utilizo en varias de las funciones 
la función "sinRepetidos" que tiene un costo cuadrático ya que es lineal en la condición del if 
(cuando utiliza "pertenece y xs") y eso lo hace para todos los elementos de la lista en la 
recursión... y esto genera el costo cuadrático.
-}

emptyS :: Set a
emptyS = S []
-- Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
addS y (S xs) = S (y:xs)
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool
belongs y (S xs) = pertenece y (sinRepetidos xs)
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = longitud (sinRepetidos xs)

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns
-- Devuelve la cantidad de elementos distintos de un conjunto.


