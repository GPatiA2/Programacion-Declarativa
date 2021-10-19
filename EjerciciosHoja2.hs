--Ej1
--a
fibonacciNesimo:: Int -> Int
fibonacciNesimo x 
   | x == 1 = 1
   | x == 0 = 1
   | otherwise = fibonacciNesimo (x-1) + fibonacciNesimo (x-2)
--b
fibonacciNesimo':: Int -> Int
fibonacciNesimo' x = fibAux 0 1 2 x 

fibAux:: Int -> Int -> Int -> Int -> Int
fibAux x y z t = if z == t then (x+y) else fibAux y (x+y) (z+1) t

-- sol con fib final 
fibonacciFinal x = fibonacciFinalAux x 0 1

fibonacciFinalAux 0 p q = p + q
fibonacciFinalAux n p q = fibonacciFinalAux (n-1) q (q+p)

--c
listFibonacci:: Int -> [Int]
listFibonacci x = listAux x 2 [1,0]

listAux:: Int -> Int -> [Int] -> [Int]
listAux x y xs
   | x == y = xs
   | otherwise = listAux x (y+1) (sum(take 2 xs):xs)
   
--Ej2   
   
zip32:: [a] -> [a] -> [a] -> [(a,a,a)]
zip32 xs xv xt = zipWith juntar xt (zipWith (,) xs xv)  where juntar x (v, u) = (x, v, u)

--imparesEN:: Integral a => [a] -> [a]
imparesEn:: [Int] -> [Int]
imparesEn xs = filter (\x -> (mod x 2 == 1)) xs

escalar:: [Int] -> [Int] -> Int
escalar xs ys = sum(zipWith (*) xs ys)

mcdList:: [Int] -> Int
mcdList xs = foldr gcd (head xs) xs

--Mejor asi porque hago menos cuentas
mcdList':: Integral a => [a] -> a
mcdList' (x:xs) = foldr gcd x xs


-- Ej 6

--a
-- (0,0) (1,2) (3,6) (7,14) (15,30)
-- El primer numero de cada pareja siempre es la iesima potencia de 2 - 1
-- El segundo numero siempre es el primero por 2
--                    Lista de las 2^i - 1 numeros desde 0 hasta infinito
--    Formo la pareja
l1 = [(x, 2*x) | x <- [(2^x)-1 | x <- [0..]]]

--b
-- La secuencia es positivo, negativo, positivo ....
-- Como los numeros impar,    par,       impar  ....
-- Simplemente multiplico cada numero del 1 al infinito por -1 elevado al siguiente numero
-- Si es par -> El siguiente es impar -> -1 ^impar es negativo
-- Si es impar -> El siguiente es par -> -1 ^par es positivo
l2 = [ x*((-1)^(x+1)) | x <- [1..]]
l2' = [if mod x 2 == 1 then x else -x | x <- [1..]]
--c
-- Simplemente aplico un filtro a la lista [1..n] segun si son pares o no
paresHastan:: Integral a => a -> [a]
paresHastan n = [x | x <- [0..n] , (\x -> ((mod x 2) == 0)) x]

paresHastan' n = [0,2..n]
--d
-- Los pares son de la forma 2*x donde x esta entre 0 y n
-- Son los n primeros pares naturales , esta bien
listpares:: Integral a => a -> [a]
listpares n  = [2*x | x <- [0..n]]

listpares'' n = [x | x <- [0..n], even x]

--e
-- Voy a construir tuplas de (x,y) donde x pertenece a xs y es par , y donde y pertenece a ys y es impar.
-- Me aprovecho de que por cada elemento que se produce en el primer generador, se producen todos los elementos en el segundo para
--    construir todas las tuplas
mezclaParImpar:: Integral a => [a] -> [a] -> [(a,a)]
mezclaParImpar xs ys  = [(x,y) | x <- filter (\x -> ((mod x 2) == 0)) xs , y <- filter (\x -> ((mod x 2) == 1)) ys]

--f
-- Puedo obtener todos los prefijos tomando [0..lenght(lista)] elementos del principio de la lista  
prefijos:: [a] -> [[a]]
prefijos xs =  [take n xs | n <- [0..length(xs)]]