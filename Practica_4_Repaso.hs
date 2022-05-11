
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