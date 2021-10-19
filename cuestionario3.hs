fact x
   | x == 0 = 1
   | otherwise = fact(x-1)*x
   
f 0 _ = 1
f n m = if n >= 0 then m else 3

factAc x = factAux x 1

factAux:: Int -> Int -> Int
factAux a b
   | a == 0 = b
   | otherwise = factAux (a-1) (b*a)
  
g:: Ord a => a -> a -> a -> a
g x y z = let m = min (min x y) z in m 