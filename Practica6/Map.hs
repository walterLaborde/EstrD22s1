module Map(Map,emptyM,assocM,lookupM,deleteM,keys)

    where

data Map k v = M [(k,v)]
{-
INV.REP.: en M kvs no hay claves repetidas.
-}

-- Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M []

Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (asociar k v kvs)

asociar :: k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [(k,v)]
asociar k v ((k',v'):kvs) = if k==k' 
                                then (k',v) : kvs
                                else (k',v') : asociar k v kvs


-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscar k kvs

buscar :: k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k'
                            then Just v'
                            else buscar k kvs


-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (borrar k kvs)

borrar :: k -> [(k,v)] -> [(k,v)]
borrar k []            =
borrar k ((k',v'):kvs) = if (k==k') 
                           then kvs
                           else (k',v') : borrar k kvs

Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys []          = []
keys ((k,v):kvs) = k : keys kvs