
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