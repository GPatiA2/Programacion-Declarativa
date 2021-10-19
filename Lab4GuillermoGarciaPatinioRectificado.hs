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

destino' p (x:xs) = foldl aplicar' p (x:xs)
aplicar':: Punto -> Direccion -> Punto
aplicar' (x, y) Arriba = (x, (y+1))
aplicar' (x, y) Abajo   = (x, (y-1))
aplicar' (x, y) Izquierda = ((x-1), y)
aplicar' (x, y) Derecha = ((x+1), y)



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
-- Las prioridades de los operadores dependen mucho de con qué otro operadores se va a usar
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
-- Así que , como ya estoy diciendo que Nat es de la clase Show, no tengo que definir el tipo de nuevo
-- Cuando hago INSTANCE TIPO1 TIPO2 WHERE para que tipo2 sea instancia de tipo1, al escribir las funciones tengo que indentar
   show x = show(natToInt x)
 
 --ej 3
 
type PReal = Float
type PImaginaria = Float

-- Un complejo se forma siempre con una parte real y una imaginaria
data Complejo = C PReal PImaginaria

-- Tambien se puede hacer definiendo funciones antes y cuando escribo el
--    instance simplemente igualo funciones.

-- suma :: Complejo -> Complejo -> Complejo
-- suma (C r1 i1) (C r2 i2) = C (r1+r2) (i1+i2)

-- Y ahora hago

-- instance Num Complejo where 
--    (+) = suma

instance Num Complejo where
   (+) (C r1 i1) (C r2 i2) = (C (r1+r2) (i1+i2))
   (-) (C r1 i1) (C r2 i2) = (C (r1-r2) (i1-i2))
   (*) (C r1 i1) (C r2 i2) = (C (r1*r2 - i1*i2) (r1*i2 + i1*r2))
   
instance Eq Complejo where
   (==) (C r1 i1) (C r2 i2) = (r1 == r2) && (i1 == i2)
   (/=) c1 c2 = not (c1 == c2)
   
instance Show Complejo where
   show (C r i) 
      | i > 0 = (show r) ++ "+" ++ show (abs i) ++ "i"
      | otherwise =  (show r) ++ "-" ++ show (abs i) ++ "i"
   
-- ej 4

class Medible a where
-- Aqui solo defino el tipo del metodo, pero no digo como funciona
-- Podria dar un comportamiento por defecto para ahorrarme tiempo despues con
--    el deriving
   medida:: a -> Int
   
instance Medible Bool where
   medida True = 1
   medida False = 0
   
-- Si 'a' es de la clase Medible, las listas de tipo a tambien seran medibles
--   y a continuacion defino como aplicaria los metodos de la clase a las listas de a
instance Medible a => Medible [a] where
   medida (x:xs) = medida x + medida xs
   
instance (Medible a, Medible b) => Medible (a, b) where
   medida (a,b) = medida a + medida b

--Este me produce una ambigüedad y no entiendo por qué   
instance Medible Int where
   medida x = 1
   
instance Medible Float where
   medida x = round x
   
instance Medible Complejo where
   medida (C r i) = round(sqrt(r^2 + i^2))
   
-- Reto: definir tipo para representar matrices de numeros reales y poner operaciones de suma y multiplicacion
-- Usar Fold y funciones de OS