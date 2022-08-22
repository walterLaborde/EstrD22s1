
-- recursion sobre listas

-- 1. sumatoria

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2. longitud

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns

-- 3. sucesores

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns

-- 4. conjuncion

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

-- 5. disyuncion

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

-- 6. aplanar

aplanar:: [[a]] -> [a]
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss

-- 7. pertenece

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e==x || pertenece e xs

-- 8. apariciones 

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = unoSi(x==e) + apariciones e xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 9. losMenoresA

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = if x<n 
                        then x : losMenoresA n xs 
                        else losMenoresA n xs 

-- 10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]

lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n 
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss

-- 11. agregarAlFinal

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12. concatenar

concatenar :: [a] -> [a] -> [a]
concatenar []     ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

-- 13. reversa

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

-- 14. zipMaximos

zipMaximos :: [Int] -> [Int] -> [Int]

zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = maximoEntre x y : zipMaximos xs ys

maximoEntre :: Int -> Int -> Int
maximoEntre x y = if x > y 
                    then x
                    else y

-- 15. elMinimo

elMinimo :: Ord a => [a] -> a
-- PRECOND : la lista no puede ser vacia 
elMinimo []     = error "la lista no puede ser vacia"
elMinimo (x:[]) = x
elMinimo (x:xs) = elMinimo xs

-- RECURSION SOBRE NUMEROS

-- 1. factorial

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2. cuentaRegresiva

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n<1
                        then []
                        else n : cuentaRegresiva(n-1)

-- 3. repetir

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e 

-- 4. losPrimeros 

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0  _     = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

-- 5. sinLosPrimeros

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs 
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- REGISTROS 

-- Personas

data Persona = P String Int
            --   Nombre Edad

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if(esMayorQueLaOtra p1 p2)
                        then p1
                        else p2


-- mayoresA

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if (edad (p) > n) 
                        then p : mayoresA n ps
                        else mayoresA n ps 

-- promedioEdad

promedioEdad :: [Persona] -> Int
promedioEdad [] = error"No se puede dividir por cero"
promedioEdad ps = div (sumatoria(edades ps)) (longitud ps)

edades :: [Persona] -> [Int]
edades (p:[]) = [edad p] 
edades (p:ps) = edad p : edades ps

-- elMasViejo

elMasViejo :: [Persona] -> Persona
elMasViejo []     = error "la lista no puede ser vacia"
elMasViejo (p:[]) = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)


-- POKEMON Y Entrenador

data TipoDePokemon = Agua | Fuego | Planta
    deriving Eq
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]


-- casos de uso 

pokeOne   = ConsPokemon Agua   28
pokeTwo   = ConsPokemon Fuego  45
pokeThree = ConsPokemon Planta 30
pokeFour  = ConsPokemon Planta 33

jorge   = ConsEntrenador "George" [pokeOne,pokeTwo,pokeThree,pokeFour]
clarisa = ConsEntrenador "Clari"  [pokeTwo,pokeThree]


-- cantPokemon 

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = longitud ps 

-- cantPokemonDe 

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador n ps) = apariciones t (tiposDePokemones ps)

tiposDePokemones :: [Pokemon] -> [TipoDePokemon]
tiposDePokemones []     = []
tiposDePokemones (p:ps) = tipo p : tiposDePokemones ps

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t e) = t

-- losQueLeGanan

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = cantPokeQueLeGanaATodos (pokesDeTipo t ps1) ps2

cantPokeQueLeGanaATodos :: [Pokemon] -> [Pokemon] -> Int
cantPokeQueLeGanaATodos []       ps2 = 0
cantPokeQueLeGanaATodos (p1:ps1) ps2 = unoSi(leGanaATodos p1 ps2) + cantPokeQueLeGanaATodos ps1 ps2

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p []       = True
leGanaATodos p (p1:ps1) = superaA p p1 && leGanaATodos p ps1

pokesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokesDeTipo t []     = []
pokesDeTipo t (p:ps) = if t == (tipo p)
                          then p : pokesDeTipo t ps
                          else pokesDeTipo t ps

-- funciones accesorias

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 _) (ConsPokemon t2 _) = esSuperiorElTipo t1 t2

esSuperiorElTipo :: TipoDePokemon -> TipoDePokemon -> Bool

esSuperiorElTipo Agua Fuego   = True
esSuperiorElTipo Fuego Planta = True
esSuperiorElTipo Planta Agua  = True
esSuperiorElTipo _      _     = False


-- esMaestroPokemon

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n [])     =  False
esMaestroPokemon (ConsEntrenador n (p:ps)) =  pertenece Agua   (tiposDePokemones (p:ps)) &&
                                              pertenece Fuego  (tiposDePokemones (p:ps)) &&
                                              pertenece Planta (tiposDePokemones (p:ps))


-- ROL Y EMPRESA

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
  -- deriving Eq
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

-- funciones de comparacion
esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto (ConsProyecto n1) (ConsProyecto n2) = n1 == n2

esMismoSeniority :: Seniority -> Seniority -> Bool
esMismoSeniority Junior     Junior     = True
esMismoSeniority SemiSenior SemiSenior = True
esMismoSeniority Senior     Senior     = True
esMismoSeniority s1         s2         = False

-- casos de uso

dev1 = Developer  Junior     (ConsProyecto "proy1")
dev2 = Developer  SemiSenior (ConsProyecto "proy1")
dev3 = Developer  Junior     (ConsProyecto "proy3")
mgm1 = Management Senior     (ConsProyecto "proy2")

ivm = ConsEmpresa [dev1,dev2,dev3,mgm1]

-- proyectos 

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinProyectosRepetidos(proyectosDeRoles rs)

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles []     = []
proyectosDeRoles (r:rs) = (proyecto r) : proyectosDeRoles rs

sinProyectosRepetidos :: [Proyecto] -> [Proyecto]
sinProyectosRepetidos []     = []
sinProyectosRepetidos (x:xs) = agregarSiHaceFaltaProyecto x (sinProyectosRepetidos xs) 

agregarSiHaceFaltaProyecto :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSiHaceFaltaProyecto p ps1 = if(estaIncluidoProyecto p ps1)
                        then ps1
                        else p : ps1 

estaIncluidoProyecto :: Proyecto -> [Proyecto] -> Bool
estaIncluidoProyecto p []     = False
estaIncluidoProyecto p (p1:ps1) = esMismoProyecto p p1 || estaIncluidoProyecto p ps1

proyecto :: Rol -> Proyecto
proyecto (Developer _ p)   = p
proyecto (Management _  p) = p

-- losDevSenior

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = losDevsSeniorEnProyectos rs ps

losDevsSeniorEnProyectos :: [Rol] -> [Proyecto] -> Int
losDevsSeniorEnProyectos []     ps = 0
losDevsSeniorEnProyectos (r:rs) ps = unoSi (esDevSenior r && trabajaEn r ps) +
                                        losDevsSeniorEnProyectos rs ps    

esDevSenior :: Rol -> Bool
esDevSenior r = esDev r && esSenior r

esDev :: Rol -> Bool
esDev (Developer  _ _) = True
esDev (Management _ _) = False

esSenior :: Rol -> Bool
esSenior (Developer  s _) = esMismoSeniority s Senior
esSenior (Management s _) = esMismoSeniority s Senior


trabajaEn :: Rol -> [Proyecto] -> Bool
trabajaEn r ps = estaIncluidoProyecto (proyecto r) ps


-- cantQueTrabajanEn

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = losEmpleadosEnProyectos rs ps

losEmpleadosEnProyectos :: [Rol] -> [Proyecto] -> Int
losEmpleadosEnProyectos []     ps = 0
losEmpleadosEnProyectos (r:rs) ps = unoSi(trabajaEn r ps) + losEmpleadosEnProyectos rs ps

--asignadosPorProyecto 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = procesarProyectos rs 

procesarProyectos :: [Rol] -> [(Proyecto, Int)]
procesarProyectos []     = []
procesarProyectos (r:rs) =   incluirElProyecto (proyecto r) (procesarProyectos rs)

incluirElProyecto :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
incluirElProyecto p []           = [(p,1)]
incluirElProyecto p ((p1,n):ps1) = if esMismoProyecto p p1 
                                     then (p1,n+1) : ps1
                                     else (p1,n) : incluirElProyecto p ps1
