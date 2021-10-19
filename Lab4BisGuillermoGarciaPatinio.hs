-- Guillermo Garcia Patinio Lenza
--ej 1

-- Tipo de datos enumerado que representa las direcciones
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Show, Ord)
-- Alias para una tupla de dos enteros
type Punto = (Int, Int)

-- Funcion que aplica una direccion a un punto
aplicar:: Direccion -> Punto -> Punto
aplicar Arriba (x, y) = (x, (y+1))
aplicar Abajo (x, y)  = (x, (y-1))
aplicar Izquierda (x, y) = ((x-1), y)
aplicar Derecha (x, y) = ((x+1), y)

--a
--Version sin recursion final
trayectoria:: Punto -> [Direccion] -> [Punto]
trayectoria p [] = []
trayectoria p (x:xs) = s : (trayectoria s xs) where s = aplicar x p  

--Version con recursion final
trayectoria':: Punto -> [Direccion] -> [Punto]
trayectoria' p xs = trayectoria'' p xs []

trayectoria'':: Punto -> [Direccion] -> [Punto] -> [Punto]
trayectoria'' p [] ys = reverse ys
trayectoria'' p (x:xs) ys = trayectoria'' s xs (s:ys) where s = aplicar x p

--b
inferior:: Int -> [Direccion] -> [Direccion] -> Bool
--  								genero una trayectoria desde x y con movs        genero otra trayectoria desde el mismo punto con movs'
--  								De esas dos trayectorias, me quedo solo con la parte dentro del plano nxn usando el takeWhile (plano n)
--   								Entiendo que cuando se sale, ya deja de contar        
-- 									Despues, para ese par de trayectorias, compruebo que la primera siempre está bajo la segunda , y devuelvo un booleano
--                                  Asi tengo un bool por cada par de trayectorias generadoas por movs y movs' desde cada punto del plano

-- Ahora hago un foldr con && empezando con True para comprobar si todos los pares de trayectorias cumplen la condicion
-- Aprovechando el foldr, en cuanto encuentra un par de trayectorias que no cumplen la condicion (un false en la lista) para
inferior n movs movs' = foldr (&&) True l where l = [(uncurry comprobar) ( takeWhile (plano n) (trayectoria' (x,y)  movs) , takeWhile (plano n) (trayectoria' (x,y)  movs')) | x <- [1..n] , y <-[1..n]  ]

-- Funcion que comprueba si un punto esta dentro de un plano nxn
plano:: Int -> Punto -> Bool
plano n (a,b) = if (a <= n && a >= (-n) && b <= n && b >= (-n)) then True else False

-- Funcion que comprueba si dadas dos listas de puntos
-- los puntos de la primera lista se mantienen siempre por debajo de los de la segunda
-- NOTA: Para el ejercicio se asume que movs y movs' tienen la misma longitud, en caso de no tenerla, se devuelve False 
comprobar:: [Punto] -> [Punto] -> Bool
comprobar [] [] = True
comprobar xs [] = False
comprobar [] xs = False
comprobar ((a,b):xs) ((c,d):ys) = if b <= d then comprobar xs ys else False

-- Extra
-- No se si quieria decir que las dos trayectorias empezaran en el mismo punto
-- Asi que esta version lo hace para cualquier par de puntos del plano n*n 
--     permitiendo que empiecen en puntos diferentes
inferior':: Int -> [Direccion] -> [Direccion] -> Bool
inferior' n movs movs' = foldr (&&) True l where l = [(uncurry comprobar) ( takeWhile (plano n) (trayectoria' p  movs) , takeWhile (plano n) (trayectoria' p1  movs')) | p <- [(a,b) | a <- [1..n], b <- [1..n]] , p1 <- [(a,b) | a <- [1..n], b <- [1..n]]  ]

-- ejercicio 2

data Tree a = Hoja a | Nodo a [Tree a] deriving Eq

t1::Tree Int
t1 = Nodo 3 [Hoja 4, Nodo 5 [Hoja 6, Hoja 7], Hoja 8]

t2::Tree Int
t2 = Nodo 3 [Hoja 1, Nodo 5 [Hoja 6, Hoja 7], Hoja 8]

--a
listaHojas:: Tree a -> [a]
listaHojas t = cuentaHojas t []

cuentaHojas:: Tree a -> [a] -> [a]
cuentaHojas (Hoja a) xs = (a:xs)
cuentaHojas (Nodo a hijos) xs = (concat $ map f hijos) where f tree = cuentaHojas tree xs


listaNodos:: Tree a -> [a]
listaNodos t = cuentaNodos t []

cuentaNodos:: Tree a -> [a] -> [a] 
cuentaNodos (Hoja a) _ = []
cuentaNodos (Nodo a hijos) xs = (:) a (concat(map f hijos)) where f tree = cuentaNodos tree xs

repMax:: Ord a => Tree a -> Tree a
repMax t = sustituir (maximoEn t) t

maximoEn:: Ord a => Tree a -> a
maximoEn (Hoja a) = a
maximoEn (Nodo a xs) = max a (maximum( map maximoEn xs))

sustituir:: a -> Tree a -> Tree a
sustituir x (Hoja t) = Hoja x
sustituir x (Nodo t xs) = (Nodo x (map (sustituir x) xs))

--b
menorEq :: Ord a => Tree a -> Tree a -> Bool
menorEq (Hoja x) (Hoja y) = x <= y
menorEq (Hoja x) (Nodo y xs) = True
menorEq (Nodo y xs) (Hoja x) = False
--                              Si el elem de x es mas grande que el de y -> falso
--                                                              Si no, voy comparando los hijos uno a uno       
menorEq (Nodo x (s:xs)) (Nodo y (t:ys)) = if x > y then False else True && (foldr (&&) True ( map (uncurry menorEq) (zip (s:xs) (t:ys))))



instance (Ord a) => Ord (Tree a) where
   (<=) = menorEq
   
--c
-- Paso el nivel de profundidad para escribir una linea con esa separacion
mostrar :: (Show a) => Tree a -> Int -> String
mostrar (Hoja e) prof = (replicate prof '\t') ++ show e ++ "\n"
mostrar (Nodo e xs) prof = (replicate prof '\t') ++ show e ++ "\n" ++ (foldl (++) "" (map f xs)) where f x = mostrar x (prof+1)

instance (Show a) => Show (Tree a) where
   show t = mostrar t 0