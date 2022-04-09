
-- recursion sobre listas

-- 1. sumatoria

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2. longitud

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns