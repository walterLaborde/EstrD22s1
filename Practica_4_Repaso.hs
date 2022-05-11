
-- 1. Pizzas

data Pizza = Prepizza
           | Capa Ingrediente Pizza
data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int

-- ejemplos de uso
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa (Aceitunas 3) 
              (Capa Salsa 
                    (Capa Jamon 
                        (Capa Queso 
                            (Capa Jamon Prepizza))))
pizza5 = Capa Queso 
              (Capa Queso 
                  (Capa Queso 
                       (Capa Queso Prepizza)))

-- cantidadDeCapas
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

--armarPizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []         = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)


-- sacarJamon
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = if esJamon ing 
                            then (sacarJamon p)   
                            else (Capa ing (sacarJamon p)) 

esJamon ::  Ingrediente -> Bool
esJamon i  = esMismoIngrediente i Jamon


--tieneSoloSalsaYQueso  
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza     = True
tieneSoloSalsaYQueso (Capa ing p) = (esSalsaOQueso ing) && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso i = esMismoIngrediente i Salsa || esMismoIngrediente i Queso


-- duplicarAceitunas
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = if sonAceitunas ing 
                                   then (Capa ing (Capa ing (duplicarAceitunas p)))
                                   else (Capa ing (duplicarAceitunas p))

sonAceitunas :: Ingrediente -> Bool 
sonAceitunas i = esMismoIngrediente i (Aceitunas (cantidadDeAceitunas i))

cantidadDeAceitunas :: Ingrediente -> Int
cantidadDeAceitunas (Aceitunas n) = n  


--cantCapasPorPizza
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) =  cantidadDeCapasDe p : cantCapasPorPizza ps

cantidadDeCapasDe :: Pizza -> (Int,Pizza)
cantidadDeCapasDe p = (cantidadDeCapas p, p)

--hayTesoro
hayTesoro :: Mapa -> Bool
hayTesoro (Fin (Cofre obs))                   = hayTesoroEntre obs
hayTesoro (Bifurcacion (Cofre obs) m1 m2) = hayTesoroEntre obs || (hayTesoro m1 || hayTesoro m2)


objetosDe :: Cofre -> [Objeto]
objetosDe (Cofre obs) = obs

hayTesoroEntre :: [Objeto] -> Bool
hayTesoroEntre []     = False
hayTesoroEntre (o:os) = esTesoro o || hayTesoroEntre os 
 
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


-- hayTesoroEn
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m                     = hayTesoroAca m
hayTesoroEn _      (Fin c)               = error"No puedo seguir caminando"  
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esMismaDir d Izq
                                                  then hayTesoroEn ds m1  
                                                  else hayTesoroEn ds m2

hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Fin         c      ) = hayTesoroEntre (objetosDe c)
hayTesoroAca (Bifurcacion c m1 m2) = hayTesoroEntre (objetosDe c)

esMismaDir :: Dir -> Dir -> Bool
esMismaDir Izq Izq = True
esMismaDir Der Der = True
esMismaDir _   _   = False


-- caminoAlTesoro
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin         c      ) = []
caminoAlTesoro (Bifurcacion c m1 m2) = avanzarSiNoHayTesoro c m1 m2 ++ (caminoAlTesoro m1) ++ (caminoAlTesoro m2)

avanzarSiNoHayTesoro :: Cofre -> Mapa -> Mapa -> [Dir]
avanzarSiNoHayTesoro c m1 m2 = if hayTesoroEntre (objetosDe c)
                                then []
                                else hayTesoroHacia m1 m2


hayTesoroHacia :: Mapa -> Mapa -> [Dir]
hayTesoroHacia m1 m2 = if hayTesoro m1
                         then [Izq]
                         else [Der]


-- caminoDeLaRamaMasLarga
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin         c      ) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = 
    laMasLargaEntre (Izq : caminoDeLaRamaMasLarga m1) (Der : caminoDeLaRamaMasLarga m2) 

laMasLargaEntre :: [Dir] -> [Dir] -> [Dir]
laMasLargaEntre xs ys = if longitud xs > longitud ys
                            then xs
                            else ys

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns

hayCaminoHacia :: Dir -> Mapa -> [Dir]
hayCaminoHacia d m = if not (esFin m) 
                        then [d]
                        else []

esFin :: Mapa -> Bool
esFin (Fin c) = True
esFin _     = False

-- tesorosPorNivel
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin         c      ) = []
tesorosPorNivel (Bifurcacion c m1 m2) = 
    singularSi Tesoro (hayTesoroEntre (objetosDe c)) : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)


singularSi :: a -> Bool -> [a]
singularSi x True = [x] 
singularSi x _    = []

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss      = yss
juntarNiveles xss      []       = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

-- todosLosCaminos
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin         c      ) = []
todosLosCaminos (Bifurcacion c m1 m2) = 
   [Izq] : consACada Izq (todosLosCaminos m1)  ++
   [Der] : consACada Der (todosLosCaminos m2)


consACada :: a -> [[a]] -> [[a]]
consACada z []       = []
consACada z (xs:xss) = (z:xs) : consACada z xss

-- /////////// NAVE ESPACIAL

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

-- sectores
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT (EmptyT       ) = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 ++ sectoresT t2

idSector :: Sector -> SectorId
idSector (S sid _ _) = sid


-- poderDePropulsion
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderesDePropulsionDe t

poderesDePropulsionDe :: Tree Sector -> Int
poderesDePropulsionDe (EmptyT       ) = 0
poderesDePropulsionDe (NodeT s t1 t2) = propulsion s + poderesDePropulsionDe t1 + poderesDePropulsionDe t2

propulsion :: Sector -> Int
propulsion (S _ cps _) = poderDeMotores cps

poderDeMotores :: [Componente] -> Int
poderDeMotores []     = 0
poderDeMotores (c:cs) = poderMotor c + poderDeMotores cs

poderMotor :: Componente -> Int
poderMotor (Motor n) = n
poderMotor _         = 0


-- barriles
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT (EmptyT       ) = []
barrilesT (NodeT s t1 t2) = barrilesDe s ++ barrilesT t1 ++ barrilesT t2

barrilesDe :: Sector -> [Barril]
barrilesDe (S _ cps _) = barrilesEn cps

barrilesEn :: [Componente] -> [Barril]
barrilesEn []     = []
barrilesEn (c:cs) = agregarSiEsAlmacen c ++ barrilesEn cs 

agregarSiEsAlmacen :: Componente -> [Barril]
agregarSiEsAlmacen (Almacen bs) = bs
agregarSiEsAlmacen _            = []


-- agregarASector
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = (N (agregarCompASector cs id t))

agregarCompASector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector 
agregarCompASector cs id EmptyT          = EmptyT 
agregarCompASector cs id (NodeT s t1 t2) = 
        (NodeT (agregarAlSector cs id s) (agregarCompASector cs id t1) (agregarCompASector cs id t2))

agregarAlSector :: [Componente] -> SectorId -> Sector -> Sector
agregarAlSector cps sid (S id cs ts) = (S id (compAgregados cps sid id cs) ts)

compAgregados :: [Componente] -> SectorId -> SectorId -> [Componente] -> [Componente]
compAgregados cps sid id cs = if sid == id 
                                then cps ++ cs
                                else cs

-- asignarTripulanteA
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ids n = asignarASectores t ids n 

asignarASectores :: Tripulante -> [SectorId] -> Nave -> Nave
asignarASectores tr ids (N t) = (N (asignarAlSector tr ids t))

asignarAlSector :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarAlSector tr (id:ids) (NodeT s t1 t2) = 
        (NodeT (agregarTripulante tr id s) (asignarAlSector tr ids t1) (asignarAlSector tr ids t2)) 

agregarTripulante :: Tripulante -> SectorId -> Sector -> Sector
agregarTripulante t id (S i cs ts) = if id == i
                                        then (S i cs (t:ts))
                                        else (S i cs ts)


-- sectoresAsignados
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t n = sectoresConTripulante t n 

sectoresConTripulante :: Tripulante -> Nave -> [SectorId]
sectoresConTripulante tr (N t) = sectoresConT tr t 

sectoresConT :: Tripulante -> Tree Sector -> [SectorId]
sectoresConT t (EmptyT)        = []         
sectoresConT t (NodeT s t1 t2) = (agregarIdSector t s) ++ sectoresConT t t1 ++ sectoresConT t t2

agregarIdSector :: Tripulante -> Sector -> [SectorId]
agregarIdSector tr (S i cs ts) = idDeSector tr i ts

idDeSector :: Tripulante -> idSector -> [Tripulante] -> [idSector]
idDeSector tr i []     = []
idDeSector tr i (t:ts) = singularSi i (tr==t) ++ (idDeSector tr i ts)


-- tripulantes
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = sinTripRepetidos(todosLosTripulantes t)

todosLosTripulantes :: Tree Sector -> [Tripulante]
todosLosTripulantes EmptyT          = []
todosLosTripulantes (NodeT s t1 t2) = tripDelSector s ++ todosLosTripulantes t1 ++ todosLosTripulantes t2 

tripDelSector :: Sector -> [Tripulante]
tripDelSector (S _ _ ts) = ts

sinTripRepetidos :: [Tripulante] -> [Tripulante]
sinTripRepetidos []     = []
sinTripRepetidos (t:ts) = agregarTripSiNoEsta t (sinTripRepetidos ts)

agregarTripSiNoEsta :: Tripulante -> [Tripulante] -> [Tripulante]
agregarTripSiNoEsta t ts = if (estaIncluidoElTrip t ts)
                            then ts
                            else t : ts

estaIncluidoElTrip :: Tripulante -> [Tripulante] -> Bool
estaIncluidoElTrip t []       = False
estaIncluidoElTrip t (t1:ts1) = (t == t1) || estaIncluidoElTrip t ts1  

