--Guillermo Garcia Patinio Lenza

-- Ejercicio 2
 
 -- last [1..10^5] regular 100000
 -- last [1..10^7] regular 10000000
 -- last [1..10^20] mucho
 -- head [1..10^20] poco 1
 -- last [10^20..1] poco 1
 -- head (tail [1..10^20]) poco 2
 -- length [1..10^20] mucho 
 -- last (take (10^7) [1..10^20]) poco 10000000
 -- head (take (10^7) ([1..100] ++ [1..10^20])) poco 1 
 -- last (take 100 ([1..10^20] ++ [1..100])) poco 100
 -- last (drop 100 ([1..10^20] ++ [1..100])) mucho 
 -- head (drop (10^7) ([1..10^20] ++ [1..100])) poco 10000001
 -- [1..10^7]==[1..10^7] poco True
 -- [1..10^20]==[1..10^20] mucho
 -- [1..10^20]==[1..10^20+1] mucho
 -- [1..10^20]==[2..10^20] poco False
 -- head (reverse [1..10^7]) regular 10000000
 -- last (reverse [1..10^7]) regular 1
 -- reverse [1..10^20] == reverse [1..10^20+1] mucho
 
 -- Ejercicio 1
anios:: Integer -> Integer
anios x = div x (3600*24*365)

-- La expresion seria anios (10`10)

 
c :: Integer -> (Integer, Integer, Integer, Integer, Integer)
c x 
   | x <= 0  = (0,0,0,0,0)
   | x > 0   = (a,d,h,m,s) where { a = anios(x) ; d = mod x (365*3600*24) ; h = mod (mod x 3600) 24 ; m = mod (mod x 60) (24*60) ; s = mod x (24*60*60)}
   
 -- Ejercicio 3
 
ac :: Num a => [a] -> a
ac [] = 0
ac (x:xs) = head(x:xs) + ac(xs)

av :: Fractional a => [a] -> a
av [] = 0
av (x:xs) = ac(x:xs) / fromIntegral(length(x:xs))

-- Para hacer la media he hecho una funcion auxiliar ac que suma los elementos de una lista
-- Utilizo esa funcion para despues dividir por la longitud de la lista y obtener la media

 -- Ejercicio 4
 -- 4.1
digitos :: Int -> Int
digitos x = if x <= 9 && x >= -9 then 1
           else 1 + digitos( div x 10)
-- Para saber los digitos que tiene un numero, cuento las veces que puedo dividirlo por 10
-- hasta que me quede un numero de un solo digitos

--4.2
reduccion :: Int -> Int
reduccion x = if(x < 0) then reduccion (- x)
              else if (x >= 0 && x <= 9) then x
              else mod x 10 + div x 10
--Para reducir un numero, voy extrayendo su ultima cifra con la funcion modulo
-- Despues, quito esa ultima cifra dividiendo entre 10

--4.3
perm :: Int -> Int
perm x = if(x == 0) then 1
         else if (x < 0) then 0
         else (perm x-1)  * x
--Para sacar el numero de permutaciones de un conjunto de n elementos calculo el factorial de n
--Esta funcion me causa stack overflow para x >= 2 y no entiendo el por que

--4.4
var :: Int -> Int -> Int
var n m = if(n <= m) then 1
          else var (n-1) m * n
--Para calcular el numero de variaciones de n elementos tomados de m en m voy multiplicando como si hiciera
-- el factorial hasta que n = m. 
--4.5
comb :: Int -> Int -> Int
comb n m = div (var n m) (perm m)
-- Calcular las combinaciones de n elementos tomados de m en m consiste en emplear las dos funciones anteriores
-- haciendo el cociente la una de la otra
-- Esta funcion da stack overflow debido a que usa la funcion de permutacion

--Ejercicio 5
infix 5 &&&
(&&&) :: Bool -> Bool -> Bool
_ &&& False = False
x &&& True = x
True &&& x = x
False &&& _ = False
--Esta es estricta en el segundo argumento

infix 5 &&&&
(&&&&) :: Bool -> Bool -> Bool
False &&&& _ = False
True &&&& x = x
_ &&&& False = False
x &&&& True = x
--Esta es esctricta en el primer argumento

infix 5 |||
(|||) :: Bool -> Bool -> Bool
_ ||| False = False
x ||| True = x
False ||| _ = False
True ||| x = x

infix 5 ||||
(||||) :: Bool -> Bool -> Bool
True |||| x = x
False |||| _ = False
x |||| True = x
_ |||| False = False

--Esta es estricta en el primer argumento

