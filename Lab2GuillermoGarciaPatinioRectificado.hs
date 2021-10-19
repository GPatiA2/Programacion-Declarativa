--Guillermo Garcia Patiño Lenza
--Ej 1

--a Lista de los cuadrados de los n primeros naturales

listSquares:: Int -> [Int]
listSquares n = eleva2 [0..n]

--eleva2 Es una funcion auxiliar que eleva al cuadrado todos los elementos de una lista
eleva2:: Num a => [a] -> [a]
eleva2 (x:xs) = (x^2) : eleva2(xs)
eleva2 [] = []

--d Suma de los menores de n que sean multiplos de 3 o 5
--Esta funcion devuelve la suma de los menores que n multiplos de 3 o 5
sumMultiplos35:: Int -> Int
sumMultiplos35 n = sumMultiploAux [1..n-1] 0

-- Esta funcion va recorriendo la lista y acumulando el resultado
sumMultiploAux:: [Int] -> Int -> Int
sumMultiploAux (x:xs) n = if (((mod x 5) == 0) || ((mod x 3) == 0)) then sumMultiploAux xs (n+x) else sumMultiploAux xs n
sumMultiploAux [] n = n

--c Suma desde i = i hasta i = n de i * |cos(i)|
-- Primer intento usando listas, llamando a sumCosAux
--sumCos:: Floating b => Int -> b
--sumCos n = sumCosAux [1..n]

-- Segundo intento sin listas, llamando a sumCosAux2
--sumCos:: Int -> Float
--sumCos n = sumCosAux2 n 0

-- Esta funcion recorre la lista calculando cada i*cos(i) y devuelve la suma
-- Lo he intentado con acumuladores, pero no compila por los tipos y no se arreglarlo
--   sumCosAux:: Floating a => [a] -> a -> a
--   sumCosAux (x:xs) a = sumCosAux (xs) (a + (x * cos(x)))
--   sumCosAux [] a = a

--Esta funcion cuenta desde 0 a n y va acumulando en el segundo parametro i*|cos i|
--sumCosAux2:: Int -> Float -> Float
--sumCosAux2 x y 
--   | x == 0 = y
--   | x > 0  = sumCosAux2 (x-1) (y + abs((x * cos(x))))

--e Numero de potencias de 3 menores que n y que acaban en 43
-- Funcion que resuelve el enunciado apoyandose en una auxiliar
acaban43:: Int -> Int
acaban43 n = contarPotencias n 1 0

-- Esta funcion va mirando si cada potencia de 3 acaba en 43 una por una y devuelve el resultado con recursion final
contarPotencias:: Int -> Int -> Int -> Int
contarPotencias n x y
   | x >= n = y
   | (x < n) && ((mod x 100) == 43) = contarPotencias n (x*3) (y+1)
   | (x < n) && ((mod x 100) /= 43) = contarPotencias n (x*3) y
   
   
-- Ej 2

--a Lista de los cuadrados de los n primeros naturales
cuadradosN:: Int -> [Int]
cuadradosN n = map cuad [0..n] where cuad t = t^2

--b Lista de pares (x^2 , x) ordenada de mayor a menor 
-- Intento usando el foldr. Me da error porque me dice que los patrones de agrupa no son exhaustivos y no se que caso me estoy dejando
-- La operacion f del fold es tomar un elemento a , construir la tupla (a, a^2) y pegarlo a la lista.
-- El elemento e del fold es la lista vacia a la que tengo que ir pegando el resto de pares
--parejaCuadrado:: Int -> [(Int,Int)]
--parejaCuadrado n = foldr agrupa [] [0..n] 

--agrupa :: Int -> [(Int, Int)] -> [(Int, Int)]
--agrupa t (x:xs) = (t, (t^2)) : (x:xs)

parejaCuadrados2:: Int -> [(Int, Int)]
parejaCuadrados2 n = reverse (zip [1..n] (map (^2) [1..n]))

--c Suma desde i = 1 hasta i = n de i*cos(i)
-- De nuevo, intento usando fold pero me pasa como en el 2, que los tipos no me cuadran bien 
-- Parece que es porque al multiplicar cos(n)*n estoy multiplicando un Floating por un Int, y eso da un Floating
-- Y no puede sumar un Floating con un Int 
--sumaCosenos:: Num b => Int -> b
--sumaCosenos n = foldr cosenoIporI 0 [1..n]

--cosenoIporI:: Num b => Int -> b -> b
--cosenoIporI n x = x + (cos(n) * n)

-- Intento usando sum y map y sigo teniendo problemas con los tipos.
--sumaCosenos2:: Int -> [Float]
--sumaCosenos2 n = map cos [1..n]

-- d Suma de los multiplos de 3 o 5 menores que n
sumMultMenores:: Int -> Int
sumMultMenores n = sum ( filter multiplo [1..(n-1)] ) where multiplo x = (((mod x 5) == 0) || ((mod x 3) == 0))

--Ejercicio 3
--a
--Construyo una tupla con primer elemento f aplicado al n-esimo de la lista y segundo elemento g aplicado al n-esimo elemento
-- y miro para cada elemento de la lista de tuplas si el primer elemento de la tupla es igual alsegundo
iguales:: (Eq a, Enum b) => (b -> a) -> (b -> a) -> b -> b -> Bool
iguales f g n m = all tuplaIguales (zip(map f [n..m]) (map g [n..m])) where tuplaIguales (x,y) = x == y

--b
-- Filtro la lista por los que cumplen la propiedad y me quedo con el minimo
menorA:: (Enum a, Ord a) => a -> a -> (a -> Bool) -> a
menorA n m p = minimum (filter p [n..m]) 

--c
-- Filtro la lista por los que cumplen la propiedad y me quedo con el maximo
mayor:: (Enum a, Ord a, Num a) => a -> (a -> Bool) -> a
mayor n p = maximum (filter p [0..n])

-- *Nota : En el apartado b, parece que Haskell infiere el tipo de los elementos de la lista porque se le pasan como parametros
--         el inicio y el final de la lista. En el apartado c estoy obligado a decir que son numeros porque solo doy el final como
--         parametro

--d 
-- Uso la funcion any para comprobar si existe algun elemento que cumple  p entre n y m
ex:: (Ord a, Enum a) => a -> a -> (a -> Bool) -> Bool
ex n m p = any p [n..m]

--Ejercicio 4

--a
-- Creo un par de dos listas una con los elementos de xs que cumplen p y otra con los elementos de xs que cumplen q
filter2:: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q = (filter p xs, filter q xs)

--b
-- Para hacer lo que pide el enunciado me apoyo en una funcion auxiliar que usa recursion final
-- Hago reverse a la lista de propiedades para que la funcion auxiliar haga las aplicaciones en el orden correcto
filters:: [a] -> [(a -> Bool)] -> [[a]]
filters xs ps = filterAux xs (reverse(ps)) [] 

-- Esta funcion va acumulando (por la izquierda) en el tercer parametro el resultado de las aplicaciones de filter psi xs que mira los elementos de xs
--  que cumplen psi.
filterAux:: [a] -> [(a -> Bool)] -> [[a]] -> [[a]]
filterAux xs [] ts = ts 
filterAux xs ps ts = filterAux xs (tail ps) ((filter (head ps) xs):ts )
