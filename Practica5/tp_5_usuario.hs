-- 1. Cálculo de costos


head’ :: [a] -> a
head’ (x:xs) = x 

{-
costo Constante porque solamente tiene que mirar el primer 
elemento de la lista... NO DEPENDE DE LA CANTIDAD DE ELEMENTOS
-}

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

{-
costo lineal porque con cada elemento (x) realiza una operación de costo CONSTANTE 
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{-
costo lineal porque con cada elemento (n) lo multiplica al factorial del anterior
-}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{-
costo lineal porque con cada elemento (x) suma 1
-}


factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

{-
costo Cuadratico porque a cada elemento (factorial x) se le realiza una operacion lineal
-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
               -- constante || costo lineal
{-
costo lineal porque con cada elemento (n) se fija en cada elemento de la lista para ver si está o no. 
-}


sinRepetidos :: Eq a => [a] -> [a]  -- costo cuadratico porque es lineal para cada x porque usa pertenece que es lineal
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs               -- costo lineal
        then sinRepetidos xs        
        else x : sinRepetidos xs



sinRepetidosR :: Eq a => [a] -> [a] -- costo cuadrático porque es lineal para cada x porque usa agregarSiNoEsta.
sinRepetidosR []     = []
sinRepetidosR (x:xs) = agregarSiNoEsta x (sinRepetidosR xs)

agregarSiNoEsta :: Eq a => a -> [a] -> [a]  -- costo lineal porque para cada elemento se fija si es igual = y : 
agregarSiNoEsta x []     = [x]
agregarSiNoEsta x (y:ys) =  if x==y    
                                    then y : ys 
                                    else y : agregarSiNoEsta x ys



{-
costo Cuadratico porque pertenece tiene costo lineal y como pregunto eso para cada elemento, deduzco que es Cuadratico 
-}


-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

{-
costo lineal porque con cada elemento (x) realiza una operación de costo CONSTANTE que es agregarlo a append xs ys
-}


concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

{-
costo lineal porque con cada elemento (x) realiza una operación de costo CONSTANTE que es agregarlo a append xs ys
-}

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

{-
lineal porque cada elemento x realiza una operación de costo constante al hacer cons con takeN
-}

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

{-
lineal porque realiza una operación de costo constante con cada n que saca
-}

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

{-
lineal porque cada una de las operaciones son lineales.
-}

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

{-
cuadratico porque min x es una operación lineal (porque busca el minimo en una lista) y esto lo hace
para todos los elementos de la lista restante.
-}

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
    if n == x
        then xs
        else x : sacar n xs

{-
lineal porque en el "else" x realiza una operacion de costo constante para cada elemento de la lista.
-}


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
    in m : ordenar (sacar m xs)

{-
cuadratico por "minimo xs"
-}


-- =============================================
-- SET
-- =============================================


-- /////////////////////
-- USUARIO
-- /////////////////////

import SetV1

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen ys s = losQueEstanDelSet (setToList s) ys

losQueEstanDelSet :: Eq a => [a] -> [a] -> [a]
losQueEstanDelSet [] _ = []
losQueEstanDelSet x:xs ys = if not (pertenece x ys)
                                then losQueEstanDelSet xs ys
                                else losQueEstanDelSet x:xs ys
 
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
al conjunto.

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (agregarListaAlSet xs emptyS)

agregarListaAlSet :: Eq a => [a] -> Set a -> Set a
agregarListaAlSet []     _ = emptyS  
agregarListaAlSet (x:xs) s = addS x (agregarListaAlSet xs s)  

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura
-- auxiliar.

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT x t1 t2) = unionS x (unionS unirTodos t1 unirTodos t2)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
del arbol.
