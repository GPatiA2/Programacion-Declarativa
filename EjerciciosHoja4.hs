-- Guillermo Garcia Patiño Lenza

-- ej1

primeroQueCumple :: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple p (x:xs) = if p x == True then Just x else primeroQueCumple p xs
primeroQueCumple p [] = Nothing

primeroQueCumple':: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple' p (x:xs) = Just (head(dropWhile (not.p) (x:xs)))
primeroQueCumple' p [] = Nothing

primeroQueCumple'' p (x:xs) = Just (head  $ filter p (x:xs))
primeroQueCumple'' p [] = Nothing

primeroQueCumple''' p (x:xs)
   | null cumplen = Nothing
   | True         = Just (head cumplen)
   where cumplen = filter p (x:xs)

-- ej2

data Cj a = Cvacio | Celems (a , Cj a)

-- Crear el cjto vacio me devuelve el Cvacio
creaCjVacio:: Cj a
creaCjVacio = Cvacio

-- Comprobar si es vacio es comparar con Cvacio
-- No entiendo por que tengo que poner Eq a
esCjVacio:: Eq a => Cj a -> Bool
esCjVacio c = c == Cvacio

-- Comprobar si un elemento esta en un conjunto se hace recursivamente
elemCj:: Eq a => a -> Cj a -> Bool
elemCj x Cvacio = False
elemCj x (Celems (t, c)) = if x == t then True else elemCj x c

-- "Recorro" el conjunto añadiendo a una lista
conjTolist:: (Cj a) -> [a]
conjTolist Cvacio = []
conjTolist (Celems (t, c)) = t : conjTolist c

instance Eq a => Eq (Cj a) where
   (==) c1 c2 = (c1 < c2) && (c2 < c1)
-- (==) c1 c2 = subcjto c1 c2 && subcjto c2 c1

instance Eq a => Ord (Cj a) where
-- Para ver si un conjunto es menor que otro, compruebo si todos los elementos del primero pertenecen al segundo
--                            Genero una lista de bools que me dice para cada elemento de c1 si esta en c2
--           Hago un foldl con && para ver si todos son True. Como es recursion final, me paro al encontrar un false
-- Realmente esto es una funcion de OS que comprueba el subconjunto
   c1 <= c2 = foldl (&&) True (map (\x -> elemCj x c2) (conjTolist c1))
-- c1 <= c2 = subcjto c1 c2

-- ej 2

data Conjunto a = C [a]

conjuntoVacio:: Conjunto a
conjuntoVacio = C []

esConjuntoVacio:: Conjunto a -> Bool
esConjuntoVacio conjuntoVacio = True
esConjuntoVacio _ = False

enConjunto::Eq a => a -> (Conjunto a) -> Bool
enConjunto x (C xs) = elem x xs

conjuntoAlista:: Eq a => Conjunto a -> [a]
conjuntoAlista (C xs) = borraDuplicado xs

--Importante que para que lo de abajo funcione, al borrar los duplicados, dejo la lista en el mismo orden
borraDuplicado:: Eq a => [a] -> [a]
borraDuplicado [] = []
borraDuplicado (x:xs) = if (elem x xs) then borraDuplicado xs else x:(borraDuplicado xs)

--borraDuplicado'::Eq a => [a] -> [a]
--borraDuplicado xs = foldl (\x y -> if(elem x y) then x else y:x) [] xs
-- Si lo hago con foldr me sale en el mismo orden 

listaAConjunto:: Eq a => [a] -> Conjunto a
listaAConjunto (x:xs) = C (borraDuplicado (x:xs))

esConjunto::Eq a => Conjunto a  -> Bool
esConjunto (C xs) = borraDuplicado xs == xs

-- Para que esto funcione, deberia tener la funcion subcjto -> hacerlo con orden superior / listas intensionales
-- Lista intensionales es mas elegante
--instance Eq a => Eq (Conjunto a) where
--   c1 == c2 = (subcjto c1 c2) && (subcjto c2 c1)
   
--instance Eq a => Ord (Conjunto a) where
--   <= = subcjto

-- Ej 2+

data Pila a b = P [Either a b]

apilar:: (Either a b) -> (Pila a b) -> (Pila a b)
apilar x (P (t:ts)) = P (x:(t:ts))  

desapilar :: (Pila a b) -> (Pila a b)
desapilar (P (x:xs)) = (P xs)

top :: (Pila a b) -> (Either a b)
top (P (x:xs)) = x

--Falta apilar en orden alterno, si tengo un a en la cima, solo puedo apilar un b y viceversa

-- Ej 3
-- No se qué pasa, por que toma como constructor la escala en el Temp

data Escala = Kelvin | Celsius | Fahrenheit deriving Eq
instance Show Escala where
   show Fahrenheit = "Fahrenheit"
   show Celsius = "Celsius"
   show Kelvin = "Kelvin"

data Temp = T Escala Float 
-- Pasaba que el tipo de datos Temp no llevaba constructor. Ponia solo Escala Float
-- Entonces tomaba Escala como el constructor de Temp. Si añado T como constructor de Temp, ya esta

escala:: Temp -> Escala
escala (T Kelvin a) = Kelvin
escala (T Celsius a) = Celsius
escala (T Fahrenheit a) = Fahrenheit

convertir:: Temp -> Escala -> Temp
convertir (T Celsius a) Celsius = (T Celsius a)
convertir (T Celsius a) Fahrenheit = T Fahrenheit (a * 1.8 + 32)
convertir (T Celsius a) Kelvin = T Celsius (a + 273)
convertir (T Fahrenheit a) Celsius = T Celsius ((a - 32) / 1.8)
convertir (T Kelvin a) Celsius = T Celsius (a - 273)
convertir t e2 = convertir (convertir t Celsius) e2

valor :: Temp -> Float
valor (T e x) = x

instance Show Temp where
   show (T e x) = show x ++ " Grados " ++ show e 
   
instance Eq Temp where
   (==) t1 t2 = valor(convertir t1 Celsius) == valor(convertir t2 Celsius) 
   
instance Ord Temp where
   (<=) (T e1 x1) (T e2 x2) =  c1 < c2 where c1 = valor (convertir (T e1 x1) Celsius)
                                             c2 = valor (convertir (T e2 x2) Celsius)
-- Ej 4

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving Eq

insert:: Ord a => a -> Arbol a -> Arbol a
insert e (Nodo x aI aD) = if (e == x) then (Nodo x aI aD)
                                        else ( if (e > x) then (Nodo x aI (insert e aD))
                                               else (Nodo x (insert e aI) aD) )
insert e Hoja = Nodo e Hoja Hoja

inOrden:: Arbol a -> [a]
inOrden Hoja = []
inorden (Nodo a aI aD) = (inorden aI) ++ a ++ (inorden aD)
