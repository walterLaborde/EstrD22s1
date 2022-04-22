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
