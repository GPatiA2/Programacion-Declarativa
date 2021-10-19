-- Guillermo Garcia Patinio Lenza

--ej 1
-- Tipo de datos enumerado que representa las direcciones
-- DERIVING DE VARIOS SE HACE ENTRE PARENTESIS CON COMAS!!!
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Show, Ord)
-- Alias para una tupla de dos enteros
type Punto = (Int, Int)

-- Funcion que aplica una direccion a un punto
aplicar:: Direccion -> Punto -> Punto
aplicar Arriba (x, y) = (x, (y+1))
aplicar Abajo (x, y)  = (x, (y-1))
aplicar Izquierda (x, y) = ((x-1), y)
aplicar Derecha (x, y) = ((x+1), y)

-- Funcion que aplica una lista de direcciones a un punto
--  Uso foldr , y uso como valor inicial el punto que me dan
destino:: Punto -> [Direccion] -> Punto
destino p (x:xs) = foldr aplicar p (x:xs) 


--ej 2
data Nat = Cero | Suc Nat deriving (Eq, Ord)



-- a 
-- Nota : al operar naturales, o me da Cero , o me da algo que empieza por Suc
--			porque son los dos unicos constructores de la clase Nat

infixr 5 +++  
(+++) :: Nat -> Nat -> Nat
-- x + 0 = x
(+++) x Cero = x
-- x pertenece a Nat -> Tiene la forma 0 o Suc(....)
-- y pertenece a Nay -> Tiene la forma y o Suc(....)
-- Los puedo sumar con +++
-- Tenga la forma que tenga x, si y tiene la forma Suc z (donde z tambien pertenece a los Nat)
-- Entonces el resultado de x +++ y es  Suc (z +++ x)
-- x + (y+1) = (x+y)+1
(+++) x (Suc y) = Suc ( x +++ y )

-- DEFINO LA MULTIPLICACION CON MAS PRIORIDAD!!!
infixr 6 ***
(***) :: Nat -> Nat -> Nat
-- x * 0 = 0
(***) x Cero = Cero
-- x * (y+1) = (x * y) + x
(***) x (Suc y) = (x *** y) +++ x

-- b
natToInt:: Nat -> Int
-- Pasar Cero a Nat es poner 0
natToInt Cero = 0
-- Tengo que deshacerme de los Suc e ir acumulando 1
--                 Int + Acaba devolviendo un int
natToInt (Suc x) = 1 + (natToInt x)

-- c 
instance Show Nat where
-- No tengo que redefinir el tipo de las funciones de la clase que sobreescribo 
-- Por ejemplo, la funcion show de la clase Show recibe una variable de un tipo que es instancia de Show
-- Así que 
 show x = show(natToInt x)
