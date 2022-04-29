
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

