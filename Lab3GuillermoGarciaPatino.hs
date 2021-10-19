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


--takeWhile'::(a -> Bool) -> [a] -> [a]
--takeWhile' p (x:xs) = foldl 

--Ej 2

--a

--foldr1':: (a -> b -> b) -> [a] -> b
--foldr1' p xs = foldr p (p (last xs)) xs

foldl1'::(a -> a -> a) -> [a] -> a
foldl1' p (x:xs) = foldl p x xs


--Ej 3
l1 = [x | x <- [1..], x <- -x]

