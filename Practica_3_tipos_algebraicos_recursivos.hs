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