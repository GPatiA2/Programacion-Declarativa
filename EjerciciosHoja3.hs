--ej 1

--a
l1 = [map (^s) [1..20] | s <- [1..10]] 

--Cada elemento de la lista es una lista formada por los numeros del 1 al 20 elevados a s 
--  Donde para cada lista s es un numero diferente del 1 al 10
--Recordar que para cada elemento del primer generador se construyen todos los elementos del segundo

-- El tipo de l1 es (Num a, Enum a) => [[a]] 
-- Necesito enumerar los elementos de la lista y tambien necesito elevarlos a algo
l1' = [[x^s | x <- [1..20]] | s <- [1..10]]

--b

-- Cada elemento de la lista es una lista de numeros elevados a un exponente del 1 al 10
--   Y para cada una de esas listas, los numeros en ella son van del 1 al 20
l2' = [[x^s | s <- [1..10]] | x <- [1..20]]


-- ej 2

f x y = map (\u -> (g u,g (u+1))) y
        where z = x * last y
              g u = (x+z)*u

f' x y = map (t' x (last y)) y
t' x y z = (g' z x y, g' (z+1) x y)
g' z x y = (x+(x*y))*z


-- ej 3

ej3a n = filter (\x -> (mod x 2 == 0)) (map (\u -> u*u) [1..n])

--ej3b n m = concat (map 

ej3b n m = concat (map f [1..n]) where f x = map (\y -> x+y) [n..m]

ej3b'' n m = concat $ map (\x -> map (\y -> x+y) [x..m]) [1..n]

ej3b' n m = [x+y | x <- [1..n], y <- [x..m]]

--ej3c

ej3c p n m = concat $ map (\x -> map (\y -> x+y) [x..m]) (filter (\z -> p(n-z)) [1..n])

--ej 4

--a

l3 = [(x,[s | s <- [1..(div x 2)] , ((mod x s) == 0)]) | x <- [19..50]]

--b
l4 = filter p [(x,[s | s <- [1..(div x 2)] , ((mod x s) == 0)]) | x <- [0..1000]] where p (x, xs) = if ((sum xs) == x) then True else False

--c

listasDeDiv:: Integral a => a -> a -> [(a,[a])]
listasDeDiv n m = [(x,[s | s <- [1..(div x 2)] , ((mod x s) == 0)]) | x <- [n..m]]

perfectosMenoresQue:: Integral a => a -> [a]
perfectosMenoresQue n = map f (filter p [(x,[s | s <- [1..(div x 2)] , ((mod x s) == 0)]) | x <- [0..n]]) where p (x, xs) = if ((sum xs) == x) then True else False
                                                                                                                f (x, xs) = x
--ej 5

minimoDesde:: Integral a => (a -> Bool) -> a -> a
minimoDesde p n = head l where l = [x | x <- [n..] , p x] 

primo:: Integral a => a -> Bool
primo x = snd( head (listasDeDiv x x)) == [1] 

minimoDesde' p n = head $ dropWhile (not.p) [n..]

