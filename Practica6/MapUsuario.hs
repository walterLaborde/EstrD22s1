import Map

-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valoresDelM m (keys m)

valoresDelM ::Map k v -> [k] -> [Maybe v]
valoresDelM _ []     = []
valoresDelM m (k:ks) = lookupM k : valoresDelM m ks


-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m     = True
todasAsociadas (k:ks) m = pertence k (keys m) && todasAsociadas ks 


todasAsociadas' :: Eq k => [k] -> Map k v -> Bool
todasAsociadas' [] m     = True
todasAsociadas' (k:ks) m = case lookupM k m of
                             Just _  -> todasAsociadas' ks
                             Nothing -> False


-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          m = emptyM
listToMap ((k,v):kvs) m = assocM k v (listToMap kvs m)


-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = listaDePares (keys m) m

listaDePares :: [k] -> Map k v -> [(k, v)]
listaDePares []     m = []
listaDePares (k:ks) m = (k, lookupM k m) : listaDePares ks m  


-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
-- la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq []          = emptyM
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of
                            Just vs -> assocM k (v : vs) (agruparEq kvs)
                            Nothing -> assocM k [v] (agruparEq kvs)


Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m = m
incrementar (k:ks) m = case lookupM k m of
                          Just v  -> assocM k (v+1) (incrementar ks m)
                          Nothing -> incrementar ks m


-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = agregarKVAlM2 (mapToList m1) m2

agregarKVAlM2 :: [(k, v)] -> Map k v -> Map k v
agregarKVAlM2 [] m          = m
agregarKVAlM2 ((k,v):kvs) m = assocM k v (agregarKVAlM2 kvs m)

