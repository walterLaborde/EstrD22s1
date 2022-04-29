
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