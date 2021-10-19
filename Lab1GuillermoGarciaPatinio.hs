-- Guillermo Garcia Patiño Lenza

--Ejercicio 1
-- a
anios :: Int -> Int
anios x = x/(3600*24*365)
   
-- b

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
 
 -- Ejercicio 3
acumula :: Num a => [a] -> a
acumula []     = 0
acumula x:xs   = x + acumula(xs)
 
media :: Fractional a => [a] -> a
media []    = 0
media x:xs  = acumula(x:xs) / fromIntegral((length(xs)))
 

 






