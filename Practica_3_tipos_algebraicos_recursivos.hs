-- TP 3

-- TIPOS RECURSIVOS SIMPLES

-- 1.1. Celdas con bolitas

data Color = Azul | Rojo 
data Celda = Bolita Color Celda | CeldaVacia

-- casos de uso

dosRojasDosAzules = Bolita Rojo 
                        (Bolita Azul 
                            (Bolita Rojo 
                                (Bolita Azul CeldaVacia)))
celda0 = CeldaVacia
celda1 = Bolita Rojo CeldaVacia
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
celda3 = Bolita Rojo (Bolita Rojo CeldaVacia)

-- nroBolitas

nroBolitas :: Color -> Celda -> Int

-- nroBolitas colour CeldaVacia     = ...
-- nroBolitas colour Bolita col cel = ... col .. nroBolitas cel 
--
nroBolitas _      CeldaVacia       = 0
nroBolitas colour (Bolita col cel) = unoSi(coloresIguales colour col) + nroBolitas colour cel 

coloresIguales :: Color -> Color -> Bool
coloresIguales Azul  Azul  = True
coloresIguales Rojo  Rojo  = True
coloresIguales c1    c2    = False

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- poner 
-- data Celda = Bolita Color Celda | CeldaVacia

poner :: Color -> Celda -> Celda
poner colour CeldaVacia       = Bolita colour CeldaVacia
poner colour (Bolita col cel) = Bolita colour (Bolita col cel)

-- Sacar

sacar :: Color -> Celda -> Celda

sacar colour CeldaVacia       = CeldaVacia
sacar colour (Bolita col cel) = if coloresIguales colour col 
                                    then cel
                                    else Bolita col (sacar colour cel)

-- ponerN

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 col cel = cel
ponerN n col cel = poner col (ponerN (n-1) col cel)

-- 1.2. Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
    deriving Eq
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

-- hayTesoro

hayTesoro :: Camino -> Bool
hayTesoro Fin           = False
hayTesoro (Cofre obs c) = hayTesoroEntre obs || hayTesoro c
hayTesoro (Nada c)      = hayTesoro c

hayTesoroEntre :: [Objeto] -> Bool
hayTesoroEntre []     = False
hayTesoroEntre (o:os) = esTesoro o || hayTesoroEntre os 
 
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--pasosHastaTesoro

pasosHastaTesoro :: Camino -> Int
-- Precond: hay al menos un tesoro

pasosHastaTesoro Fin           = 0
pasosHastaTesoro (Cofre obs c) = unoSi(not(hayTesoroEntre obs)) + pasosHastaTesoro c
pasosHastaTesoro (Nada      c) = 1 + pasosHastaTesoro c

-- hayTesoroEn 
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n c = tantosPasosAlTesoro n c

tantosPasosAlTesoro :: Int -> Camino -> Bool
tantosPasosAlTesoro n c = n == (pasosHastaTesoro c)

-- alMenosNTesoros CORREGIDO

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 c               = True
alMenosNTesoros n c               = if ((tesorosEnCaminoActual c)-n) >= 0 
                                        then True
                                        else False

darUnPaso :: Camino -> Camino
darUnPaso Fin           = Fin
darUnPaso (Cofre obs c) = c
darUnPaso (Nada      c) = c

tesorosEnCaminoActual :: Camino -> Int
tesorosEnCaminoActual Fin           = 0
tesorosEnCaminoActual (Cofre obs c) = tesorosEn obs + tesorosEnCaminoActual c
tesorosEnCaminoActual (Nada      c) = tesorosEnCaminoActual c 

tesorosEn :: [Objeto] -> Int
tesorosEn os = apariciones Tesoro os

-- cantTesorosEntre :: Int -> Int -> Camino -> Int PENDIENTE !!

-- ==================

-- 2. Tipos arbóreos
-- 2.1. Árboles binarios

-- sumarT
sumarT :: Tree Int -> Int

sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2

-- sizeT 
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

-- mapDobleT
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT x t1 t2) = NodeT (2*x) (mapDobleT t1) (mapDobleT t2)

-- perteneceT
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT z EmptyT          = False
perteneceT z (NodeT x t1 t2) = z==x || perteneceT z t1 || perteneceT z t2

-- aparicionesT
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT z EmptyT          = 0
aparicionesT z (NodeT x t1 t2) = unoSi(z==x) + aparicionesT z t1 + aparicionesT z t2

-- leaves
leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x t1 t2) = x : leaves t1 ++ leaves t2

-- heightT
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + masProfundoEntre (heightT t1) (heightT t2)

masProfundoEntre :: Int -> Int -> Int 
masProfundoEntre n1 n2 = if n1>n2 
                            then n1
                            else n2

--mirrorT
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

-- toList
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

-- levelN
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x _  _ ) = [x]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- listPerLevel
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = (x:[]) : juntarNiveles (listPerLevel t1) (listPerLevel t2)  

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss      = yss
juntarNiveles xss      []       = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

-- ramaMasLarga
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = laMasLargaEntre t1 t2

laMasLargaEntre :: Tree a -> Tree a -> [a]
laMasLargaEntre t1 t2 = if (sizeT t1 > sizeT t2)
                            then leaves t1
                            else leaves t2