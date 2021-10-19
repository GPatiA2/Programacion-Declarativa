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
--sumCos:: Floating b => Int -> b
--sumCos n = sumCosAux [1..n] 

-- Esta funcion recorre la lista calculando cada i*cos(i) y devuelve la suma
-- Lo he intentado con acumuladores, pero no compila por los tipos y no se arreglarlo
--   sumCosAux:: Floating a => [a] -> a -> a
--   sumCosAux (x:xs) a = sumCosAux (xs) (a + (x * cos(x)))
--   sumCosAux [] a = a

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
parejaCuadrados2 n = reverse ( zip [0..n] (listSquares n))

--c Suma desde i = 1 hasta i = n de i*cos(i)
-- De nuevo, intento usando fold pero me pasa como en el 2, que los tipos no me cuadran bien 
-- Parece que es porque al multiplicar cos(n)*n estoy multiplicando un Floating por un Int, y eso da un Floating
-- Y no puede sumar un Floating con un Int 
--sumaCosenos:: Num b => Int -> b
--sumaCosenos n = foldr cosenoIporI 0 [1..n]

--cosenoIporI:: Num b => Int -> b -> b
--cosenoIporI n x = x + (cos(n) * n)

-- Intento usando sum y map y sigo teniendo problemas con los tipos.
--sumaCosenos2:: Num b => Int -> b
--sumaCosenos2 n = sum ( map cosIporI2 [1..n]) where cosIporI2 t = cos(t) * t

-- d Suma de los multiplos de 3 o 5 menores que n
--Funciona mal, tengo que coregirlo
--sumMultMenores:: Int -> Int
--sumMultMenores n = sum ( filter multiplo [1..n] ) where multiplo x = if (((mod x 5) == 0) || ((mod x 3) == 0)) then True else False