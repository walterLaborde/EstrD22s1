
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

zipMaximos xs     []     = xs
zipMaximos []     ys     = ys
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
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva(n-1)

-- 3. repetir

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e 

-- 4. losPrimeros 

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0  _     = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs