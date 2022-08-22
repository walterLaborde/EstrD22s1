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
poner col c = Bolita col c

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
pasosHastaTesoro Fin           = error"hay al menos un tesoro"
pasosHastaTesoro (Cofre obs c) = contarSiNoHayTesoro obs (pasosHastaTesoro c)
pasosHastaTesoro (Nada      c) = 1 + pasosHastaTesoro c
 
contarSiNoHayTesoro :: [Objeto] -> Int -> Int 
contarSiNoHayTesoro obs n = if hayTesoroEntre obs
                              then 0
                              else 1 + n

-- hayTesoroEn 
-- Precond: Int no puede ser negativo
hayTesoroEn :: Int -> Camino -> Bool

hayTesoroEn 0 c             = hayTesoroEnCaminoActual c 
hayTesoroEn n Fin           = False
hayTesoroEn n (Cofre obs c) = hayTesoroEntre obs || hayTesoroEn (n-1) c
hayTesoroEn n (Nada      c) = hayTesoroEn (n-1) c

hayTesoroEnCaminoActual :: Camino -> Bool
hayTesoroEnCaminoActual (Cofre obs c) = hayTesoroEntre obs
hayTesoroEnCaminoActual _             = False

-- alMenosNTesoros
-- Precond : int no puede ser negativo
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin           = False
alMenosNTesoros n (Cofre obs c) = n <= cantDeTesorosEn obs || alMenosNTesoros (n-(cantDeTesorosEn obs)) c
alMenosNTesoros n (Nada      c) = alMenosNTesoros n c

cantDeTesorosEn :: [Objeto] -> Int
cantDeTesorosEn []     = 0
cantDeTesorosEn (o:os) = unoSi(esTesoro o) + cantDeTesorosEn os

-- (desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PRECOND : el primer int es menor o igual al segundo
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre x y c =  contarTesorosSiLlegueA x y 0 c 

contarTesorosSiLlegueA :: Int -> Int -> Int -> Camino -> Int
contarTesorosSiLlegueA x y z Fin           = 0
contarTesorosSiLlegueA x y z (Cofre obs c) = sumarSi(z>=x && z<=y) obs (contarTesorosSiLlegueA x y (z+1) c)
contarTesorosSiLlegueA x y z (Nada      c) = contarTesorosSiLlegueA x y (z+1) c

sumarSi :: Bool -> [Objeto] -> Int -> Int
sumarSi False _ y = y
sumarSi True obs y  = cantDeTesorosEn obs + y

-- ==================

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

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
leaves (NodeT x t1 t2) = singularSi x (esArbolVacio t1 && esArbolVacio t2)  ++ leaves t1 ++ leaves t2

esArbolVacio :: Tree a -> Bool
esArbolVacio EmptyT = True
esArbolVacio _      = False

singularSi :: a -> Bool -> [a]
singularSi x True = [x] 
singularSi x _    = []


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
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : laMasLargaEntre (ramaMasLarga t1) 
                                                   (ramaMasLarga t2)

laMasLargaEntre :: [a] -> [a] -> [a]
laMasLargaEntre xs ys = if (longitud xs > longitud ys)
                            then xs
                            else ys
                            
longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns


--todosLosCaminos
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1)  
                                        ++ consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada z []       = []
consACada z (xs:xss) = (z:xs) : consACada z xss

-- 2.2. Expresiones Aritméticas

data ExpA = Valor Int
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA

-- eval
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum  ex1 ex2) = eval ex1 + eval ex2
eval (Prod ex1 ex2) = eval ex1 * eval ex2
eval (Neg  ex1    ) = (- eval ex1)

-- simplificar

simplificar :: ExpA -> ExpA
simplificar (Sum   ex1 ex2) = simplificarSuma (simplificar ex1) (simplificar ex2)
simplificar (Prod  ex1 ex2) = simplificarProd (simplificar ex1) (simplificar ex2)
simplificar (Neg   ex1)     = agregarNegSiCorresponde(simplificar ex1)
simplificar (Valor ex)    = Valor ex

agregarNegSiCorresponde :: ExpA -> ExpA
agregarNegSiCorresponde (Neg ex) = ex 
agregarNegSiCorresponde      ex  = Neg ex 

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma  (Valor 0) z   = z
simplificarSuma  y (Valor 0)   = y 
simplificarSuma  y         z   = Sum y z

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd  (Valor 0) z   = Valor 0
simplificarProd  y (Valor 0)   = Valor 0
simplificarProd  (Valor 1) z   = z
simplificarProd  y (Valor 1)   = y 
simplificarProd  y         z   = Prod y z