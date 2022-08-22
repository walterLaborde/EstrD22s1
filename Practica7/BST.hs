module Bst (Bst, belongsBST, insertBST, deleteBST, splitMinBST, splitMaxBST,
            esBST, elMaximoMenorA, elMinimoMayorA, balanceado)

    where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    {-
    Inv Rep: 
        Invariante de BST: en (NodeT x ti td)
            todos los elementos de ti son menores que x
            todos los elementos de td son mayores que x
            ti y td también cumplen el invariante de BST
    -}

-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST y t = buscarBST y t

buscarBST :: Ord a => a -> Tree a -> Bool
buscarBST y EmptyT          = False
buscarBST y (NodeT x ti td) = 
    if(y==x)          then True
        else if (y<x) then buscarBST y ti
                      else buscarBST y td


Propósito: dado un BST inserta un elemento en el árbol.
Costo: O(log N)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST y EmptyT          = 
insertBST y (NodeT x ti td) = 
    if(y==x)          then NodeT y ti td
        else if (y<x) then NodeT x (insertBST y ti) td
                      else NodeT x ti (insertBST y td)


Propósito: dado un BST borra un elemento en el árbol.
Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x t = borrarBST x t

borrarBST :: Ord a => a -> Tree a -> Tree a
borrarBST y EmptyT          = 
borrarBST y (NodeT x ti td) = 
    if (y==x)         rearmarBST ti td
        else if (y<x) then borrarBST y ti
                      else borrarBST y td

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = td
rearmarBST ti     td = NodeT (maxBST ti) (delMaxBST ti) td

maxBST :: Ord a => Tree a -> a
maxBST (NodeT x _ EmptyT) = x      -- entre x ti td... el mas grande siempre es x
maxBST (NodeT _ _     td) = maxBST td -- como es BST los mas grandes estan en el subarbol dereccho SIEMPRE

delMaxBST :: Ord a => Tree a -> Tree a
delMaxBST (NodeT _ ti EmptyT) = ti  -- si no hay subarbol derecho, entre x y ti, el max es x... lo saco
delMaxBST (NodeT x ti     td) = NodeT x ti (delMaxBST td) -- el arbol sin el max del subarbol derecho


Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti     td) = let (min, ti') = splitMinBST ti
                                   in (min, NodeT x ti' td)


Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti     td) = let (max, td') splitMaxBST td
                                   in (max, NodeT x ti td') 


-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST :: Ord a => Tree a -> Bool
esBST EmptyT          = True
esBST (NodeT x ti td) = (maxBST ti < x) && (x < maxBST td) 


-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x EmptyT          = 
elMaximoMenorA x (NodeT y ti td) =  

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)

balanceado :: Tree a -> Bool
Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
Costo: O(N2)