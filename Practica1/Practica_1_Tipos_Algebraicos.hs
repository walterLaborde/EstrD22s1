--1. Números enteros

-- 1. Defina las siguientes funciones:

-- a) 
sucesor :: Int -> Int
-- Dado un número devuelve su sucesor

sucesor n = n+1

-- b) 
sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.

sumar n m = n+m

--c) 
divisionYResto :: Int -> Int -> (Int, Int)
{- Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}

divisionYResto n m = (div n m,mod n m)

--d) 
maxDelPar :: (Int,Int) -> Int
-- Dado un par de números devuelve el mayor de estos.

maxDelPar (n,m) = if (n>m)
                    then n 
                    else m


-- 2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión
-- a todas las funciones del punto anterior.
-- Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

-- maxDelPar(divisionYResto 40 (sucesor 3))

-- maxDelPar(divisionYResto (sumar 9 1) (sucesor 0))

-- sumar 0 (maxDelPar(divisionYResto (sumar 9 1) (sucesor 0)))


--2. Tipos enumerativos

-- 1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
-- las siguientes funciones:

data Dir = Norte | Este | Sur | Oeste

-- a) 
opuesto :: Dir -> Dir
--Dada una dirección devuelve su opuesta.

opuesto d = case d of
    Norte -> Sur
    Este  -> Oeste
    Sur   -> Norte
    Oeste -> Este 

-- b) 
iguales :: Dir -> Dir -> Bool
-- Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.

iguales Norte Norte = True
iguales Este Este   = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales d1    d2    = False    

-- c) 
siguiente :: Dir -> Dir

siguiente d = case d of 
    Norte -> Este
    Este  -> Sur
    Sur   -> Oeste
    Oeste -> Norte 

{- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
total o parcial? ¿Por qué? -}

-- 2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
-- Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
-- es domingo. Luego implementar las siguientes funciones:

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

-- a) 
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- Devuelve un par donde la primera componente es el primer día de la semana, y la
-- segunda componente es el último día de la semana.

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

primeroYUltimoDia = (primerDia, ultimoDia)

--b) 
empiezaConM :: DiaDeSemana -> Bool
--Dado un dia de la semana indica si comienza con la letra M.

{-
opción 1
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM otroDia   = False
-}

-- opción 2

empiezaConM d = esMartesOMiercoles d

esMartesOMiercoles :: DiaDeSemana -> Bool
esMartesOMiercoles Martes    = True
esMartesOMiercoles Miercoles = True 
esMartesOMiercoles otroDia   = False


--c) 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
--Dado dos dias de semana, indica si el primero viene después que el segundo.

vieneDespues d1 d2 = diaEnNumero d1 > diaEnNumero d2

diaEnNumero :: DiaDeSemana -> Int

diaEnNumero Lunes     = 1
diaEnNumero Martes    = 2
diaEnNumero Miercoles = 3
diaEnNumero Jueves    = 4
diaEnNumero Viernes   = 5
diaEnNumero Sabado    = 6
diaEnNumero Domingo   = 7


--d) 
estaEnElMedio :: DiaDeSemana -> Bool
--Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.

estaEnElMedio d = not (esLunesODomingo d)

esLunesODomingo :: DiaDeSemana -> Bool
esLunesODomingo Lunes = True
esLunesODomingo Domingo = True
esLunesODomingo otroDia = False

--3 BOOLEANOS

--a) 
negar :: Bool -> Bool
--Dado un booleano, si es True devuelve False, y si es False devuelve True.
--En Haskell ya está definida como not.

negar True  = False
negar False = True

--b) 
implica :: Bool -> Bool -> Bool
--Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
--devuelve True.
--Nota: no viene implementada en Haskell.

implica True False = False
implica _    _     = True

--c) 
and :: Bool -> Bool -> Bool
--Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
--En Haskell ya está definida como \&\&.

and True b = b
and _    _ = False

--d) 
or :: Bool -> Bool -> Bool
--Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
--En Haskell ya está definida como ||.

or True b = True
or _    b = b


-- 3 REGISTROS

data Persona = P String Int
             --  Nombre edad

nombre :: Persona -> String
--Devuelve el nombre de una persona

nombre (P n e) = n

edad :: Persona -> Int
--Devuelve la edad de una persona

edad (P n e) = e

crecer :: Persona -> Persona
--Aumenta en uno la edad de la persona.

crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.

cambioDeNombre newN (P n e) = P newN e

yo = P "Walter" 45

esMayorQueLaOtra :: Persona -> Persona -> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.

esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
--Dadas dos personas devuelve a la persona que sea mayor.

laQueEsMayor p1 p2 = if(esMayorQueLaOtra p1 p2)
                        then p1
                        else p2

sil = P "Silvi" 36

-- POKEMON

data Pokemon = Pok TipoDePokemon Int
            --     TipoDePokemon Energia

data TipoDePokemon = Agua | Fuego | Planta

data Entrenador = E String Pokemon Pokemon

superaA :: Pokemon -> Pokemon -> Bool
--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

-- una opcion 1 paso a los Pokemones por parametro y le saco el tipo del otro lado del =
-- pero eso me obliga a generar la funcion observadora "TipoDePokemon"

--superaA p1 p2 = esSuperiorElTipo (tipo p1) (tipo p2)

-- en esta opcion 2 DESARMO EL REGISTRO... es decir, digo: del Pokemon que recibo como 
-- argumento.. solo me interesa el TipoDePokemon y el otro parametro no.. y comparo los Tipos
superaA (Pok t1 _) (Pok t2 _) = esSuperiorElTipo t1 t2

esSuperiorElTipo :: TipoDePokemon -> TipoDePokemon -> Bool

esSuperiorElTipo Agua Fuego   = True
esSuperiorElTipo Fuego Planta = True
esSuperiorElTipo Planta Agua  = True
esSuperiorElTipo _      _     = False

-- aca hago el observador para la opcion de solucion 1 
tipo :: Pokemon -> TipoDePokemon
tipo (Pok t e) = t

pokeOne = Pok Agua 28
pokeTwo = Pok Fuego 33

jorge = E "George" pokeOne pokeTwo
clarisa = E "Clari" pokeTwo pokeTwo

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.

cantidadDePokemonDe t (E n p1 p2) = unoSi(esDelMisoTipo t (tipo p1)) 
                                  + unoSi(esDelMisoTipo t (tipo p2))   

esDelMisoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMisoTipo Agua Agua     = True
esDelMisoTipo Fuego Fuego   = True
esDelMisoTipo Planta Planta = True
esDelMisoTipo _      _      = False

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.

juntarPokemon (e1,e2) = pokemonDe e1 ++ pokemonDe e2

pokemonDe :: Entrenador -> [Pokemon]
pokemonDe (E n p1 p2) = p1:p2:[]

-- 4 FUNCIONES POLIMORFICAS

--a) 

loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.

loMismo x = x

--b) 

siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.

siempreSiete x = 7

--c) 
swap :: (a,b) -> (b, a)
--Dadas una tupla, invierte sus componentes.

swap (x,y) = (y,x)

--¿Por qué existen dos variables de tipo diferentes?
--2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?

{-
Son polimórficas porque puedo completar los parámetros con cualquier tipo de dato. Es decir, 
cada función NO está RESTRINGIDA a un tipo de dato específico y con eso doy por sentado que no me 
interesa trabajar con el dato.
-}

-- 5 PATTERN MATCHING SOBRE LISTAS

--1. 

--Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
--utilizar las funciones que ya vienen con Haskell):

--2. 

estaVacia :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Definida en Haskell como null.

estaVacia [] = True
estaVacia _  = False

--3. 

elPrimero :: [a] -> a
--Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
--Nota: tener en cuenta que el constructor de listas es :

elPrimero (x:_) = x
elPrimero _     = error "No puede ser lista Vacia"

--4. 

sinElPrimero :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :

sinElPrimero (x:xs) = xs
sinElPrimero _      = error "No puede ser lista Vacia"


--5. 

splitHead :: [a] -> (a, [a])
--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--Nota: tener en cuenta que el constructor de listas es :

splitHead (x:xs) = (x,sinElPrimero(x:xs))
splitHead _      = error "No puede ser lista vacia"
