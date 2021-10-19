--Guillermo Garcia Patiño Lenza

--Ej 1

--a

--La operacion f consiste en, teniendo el elemento anterior de la lista
-- y el siguiente, quedarse con el siguiente.
--Si esto lo aplico de izquierda a derecha, al final consigo quedarme con el ultimo
last'::[a] -> a
last' (x:xs) = foldl (\x y -> y) x xs

--b
-- La operacion que paso como parametro a fold consiste en coger el primer elemento
--  desde la izquierda y añadirlo a la lista por la izquierda.
-- Aplicando esta operacion de izquierda a derecha, consigo darle la vuelta a la lista
reverse':: [a] -> [a]
reverse' (x:xs) = foldl (\xv t -> t:xv) [] (x:xs)

--c
--En este caso, la operacion que paso como parametro al fold es la propiedad que paso como
-- parametro a all, y lo que se realiza por cada elemento de la lista es calcular si cumple la
-- propiedad y acumular con el booleano que ya tengo del resto de elementos

all'::(a -> Bool) -> [a] -> Bool
all' p (x:xs) = foldr (\x y -> (p x) && y) True (x:xs)

--d
-- Tomo x como primer minimo. Por cada elemento de la lista actualizo el minimo al menor valor
--  entre el que estoy mirando y el minimo de todos los anteriores. Se recorre la lista de izquierda a derecha.
minimum':: Ord a => [a] -> a
minimum' (x:xs) = foldl (\x y -> min x y) x xs

--e
-- Recorro la lista de derecha a izquierda, y a cada elemento le aplico f y lo voy pegando por la
--  izquierda en la lista.
map':: (a -> b) -> [a] -> [b]
map' f (x:xs) = foldr (\t xt -> (f t):xt) [] (x:xs)

--f 
-- Recorro la lista de derecha a izquierda añadiendo a una lista vacia por la izquierda los elementos
--  de x:xs que cumplen p

filter'::(a -> Bool) -> [a] -> [a]
filter' p (x:xs) = foldr (\t xt -> if p t then t:xt else xt) [] (x:xs)

--g
-- Para implementar el takeWhile recorro la lista de derecha a izquierda añadiendo a la lista los elementos
--  a la lista que cumplen la propiedad. Si me encuentro con alguno que no cumple la propiedad, vacio la lista

takeWhile'::(a -> Bool) -> [a] -> [a]
takeWhile' f xs = foldr (\x xv -> if f x then x : xv else []) [] xs


--EXTRA
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (\x l -> x:l) ys xs

--Ej 2

--a

-- Voy dejando operaciones pendientes desde la izquierda hasta la derecha y las realizo en el orden contrario
foldr1':: (a -> a -> a) -> [a] -> a
foldr1' p [x] = x
foldr1' p (x:xs) = p x (foldr1' p xs)

-- Este es igual que el foldl en el que uso como elemento base el primero de la lista
foldl1'::(a -> a -> a) -> [a] -> a
foldl1' p (x:xs) = foldl p x xs



-- Ej 3
-- Voy multiplicando los elementos del 1 al 10 por -1 o por 1 añadiendolos a la lista
l1 =  [x * ( (-1) ^y ) | x <- [1..10] , y <- [2,1] ]
l1'' = foldl (\xs x -> xs ++ [x,(-x)]) [] [1..100]
l1' = concat [[k, (-k)] | k <- [1..]]

-- N es el valor que suman los dos valores de la tupla
-- Asi que para cada valor de n voy construyendo tuplas con (x, n-x) donde x va desde 0 hasta n
l2 = [ (x, abs(n-x)) | n <- [0..4] , x <- [0..n]]
l2' = [(x,y) | z <- [0..], x <- [0..z], y <- [0..z], x+y == 0]

-- Ej 4

--a
-- Cada sufijo sale de quitar n elementos del principio de la lista 
sufijos:: [a] -> [[a]]
sufijos xs = [drop n xs | n <- [0..length(xs)]]

--b
-- Una sublista son elementos seguidos. Para sacar las sublistas voy quitando elementos del principio de la lista
--  y entonces produzco sublistas aplicando take.
-- No se como evitar que [] me salga repetido
sublistas:: [a] -> [[a]]
sublistas xs = [take t (drop d xs) | d <- [0..(length xs)] , t <- [1..((length xs) - d)]] 

--c
--La funcion todaslasPos coloca el primer argumento x en todas las posiciones donde puede ir en la lista (y:ys)
--Devuelve una lista de listas en la que cada elemento es la lista que se pasa como argumento a la que se le añade x en una posicion distinta
todaslasPos:: a -> [a] -> [[a]]
todaslasPos x [] = [[x]]
todaslasPos x (y:ys) = (x:y:ys) : [y:zs | zs <- todaslasPos x ys]

-- Para hacer las permutaciones de una lista, coloco el primer elemento de la lista en todas las posiciones de la lista, y para cada una de las listas
--  que resultan de hacer eso, repito el proceso.
permutaciones:: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = concat [todaslasPos x ys | ys <- permutaciones xs]

--d
-- La idea es que dados los sumandos de n-1 , puedo formar los de n si a cada sumando de n-1 le sumo 1.
-- Hay dos maneras de sumar 1: añadir un 1 a la lista (segunda parte del ++)
--							   sumar 1 a algun numero de la lista (primera parte del ++) (elegimos el primer numero siempre)
-- Tengo que hacer estas dos cosas por cada sumando de n-1
-- Ademas tenemos un caso base, en el que sabemos que los sumandos del 1 es solo el 1
-- Ejemplo : sumandos 3 provoca 1 llamada recursiva a sumandos 2, que a su vez provoca una llamada a sumandos 1
-- 			 sumandos 1 devuelve [[1]]
--			 para sacar los sumandos de 2 a partir de los de 1 ->  añado 1 a la lista -> [1,1]
--                                                               ->  sumo 1 a algun numero de la lista -> [2]
--           ahora sumandos 2 devuelve [[1,1][2]]
--           para sacar los sumandos de 3 a partir de los de 2 -> añado 1 a la lista -> [[1,1,1],[1,2]]
--                                                             -> sumo 1 a algun numero de la lista -> [[2,1],[3]]
--           Ya tengo los sumandos de 3 [[1,1,1],[1,2],[2,1],[3]]   

sumamdos :: (Integral a) => a -> [[a]]
sumandos 1 = [[1]]
sumandos n = [(\(v:xv) -> (1+v):xv) x | x <- ns] ++ [(\xs -> 1:xs) x | x <- ns]
  where ns = sum1 (n-1)



