-- Guillermo Garcia Patiño Lenza

-- Ejercicio 1  -> definir la estructura de datos
data Rel a = R[(a,a)] deriving (Read, Show)

esRelacion:: Eq a => Rel a -> Bool
esRelacion (R xs) = compruebaRepetidos xs []

-- Comprobar para cada par de la relacion si se encuentra repetido
--   En cuanto encuentra uno repetido, da False
compruebaRepetidos:: Eq a => [(a,a)] -> [(a,a)] -> Bool
compruebaRepetidos [] xs = True
compruebaRepetidos (x:xs) ys = if elem x ys then False else compruebaRepetidos xs (x:ys)

-- Ejercicio 2 -> Hacer instancia de Eq
-- Este metodo comprueba si las dos relaciones son iguales
--   Comprueba para cada elemento de una relacion, si es elemento de la otra
--   Emplea foldr para que por ev.perezosa se deje de calcular en cuantro encuentre un elemento que 
--      no esta en la otra relacion
relacionIgual :: Eq a => Rel a -> Rel a -> Bool
relacionIgual (R xs) (R ys) = esRelacion (R xs) && esRelacion (R ys) && foldr (&&) True [elem x ys | x <- xs] && foldr (&&) True [elem x xs | x <- ys]  

instance Eq a => Eq (Rel a) where
    (==) (R xs) (R ys) = relacionIgual (R xs) (R ys)

-- Ejercicio 3 -> Programar funciones sobre el tipo de datos relacion

-- Obtiene una lista con el primer elemento de cada par de la relacion
--   y le quita los repetidos 
dominio:: Eq a => Rel a -> [a]
dominio (R xs) = quitaRepetidos [x | (x,y) <- xs] [] 

-- Funcion para quitar los repetidos de una lista
quitaRepetidos:: Eq a => [a] -> [a] -> [a]
quitaRepetidos [] xs = xs
quitaRepetidos (x:xs) ys = if elem x ys then quitaRepetidos xs ys else quitaRepetidos xs (x:ys)

-- Para una lista de pares, crea dos listas, una con los primeros elementos y otra con los segundos
--   y le quita los repetidos
elementos:: Eq a => Rel a -> [a]
elementos (R xs) = quitaRepetidos ( (++) (fst x) (snd x) ) [] where x = unzip xs

-- Hace exactamente lo mismo que la funcion anterior
soporte :: Eq a => Rel a -> [a]
soporte = elementos

-- Comprueba si una relacion es de equivalencia
--  Para ello comprueba si es una relacion realmente, si es reflexiva, si es simetrica y si es transitiva
relEquivalencia:: Eq a => Rel a -> Bool
relEquivalencia (R xs) = esRelacion (R xs) && relSimetrica (R xs) && relReflexiva (R xs) && relTransitiva (R xs)

-- En las siguientes 3 funciones se emplea foldr para que por evaluacion perezosa, 
--    se pare la comprobacion al encontrar un falso

-- Comprueba si una relacion es reflexiva
--   Para ello verifica que para todo elemento e del soporte, el par (e,e) esta en la relacion
relReflexiva:: Eq a => Rel a -> Bool
relReflexiva (R xs) = foldr (&&) True [elem (x,x) xs | x <- soporte (R xs)]

-- Comprueba si una relacion es simetrica
--   Verifica que para cada par (e1,e2) de la relacion, el par (e2,e1) esta en la relacion
relSimetrica:: Eq a => Rel a -> Bool
relSimetrica (R xs) = foldr (&&) True [elem (y,x) xs | (x,y) <- xs]

-- Comprueba si una relacion es transitiva
--   Verifica que para cada elemento y relacionado con uno del dominio x, 
--     el par (x,z) esta en la relacion (donde z es cada elemento relacionado con y)
relTransitiva:: Eq a => Rel a -> Bool
relTransitiva (R xs) = foldr (&&) True [elem (x,z) xs | x <- dominio (R xs) , y <- relacionadosCon x xs , z <- relacionadosCon y xs]

-- Devuelve una lista con los elementos relacionados con x en la lista de pares [(a,a)]
relacionadosCon:: Eq a => a -> [(a,a)] -> [a]
relacionadosCon x xs = relacionadosAux x xs []

-- Funcion auxiliar que permite hacer la anterior por recursion final
relacionadosAux:: Eq a => a -> [(a,a)] -> [a] -> [a]
relacionadosAux e [] ys = ys
relacionadosAux e ((x,y):xs) ys = if e == x && (not (elem y ys)) then relacionadosAux e xs (y:ys) else relacionadosAux e xs ys

-- Funcion que obtiene el conjunto cociente de una relacion
--   Comprueba si la relacion que se le pasa como parametro es una relacion,
--       y si lo es, obtiene el cjto cociente por recursion final
conjCociente:: Eq a => Rel a -> [[a]]
conjCociente (R xs) = if relEquivalencia (R xs) then sacarConj xs (elementos (R xs)) [] else []

-- Esta funcion calcula el conjunto cociente de una lista de pares de una relacion que se pasa como
--  parametro
-- Elemento a elemento, comprueba si ese elemento ya se encuentra en alguna de las clases de equivalencia
-- Si el elemento se encuentra en alguna clase, pasa al siguiente, y si no, agrega una lista con los elementos
--   de su clase al conjunto cocientes
sacarConj:: Eq a => [(a,a)] -> [a] -> [[a]] -> [[a]]
sacarConj xs [] ys = ys
sacarConj xs (z:zs) ys = if elegido z ys then sacarConj xs zs ys  else sacarConj xs zs (t:ys) where t = (relacionadosCon z xs)

-- Funcion auxiliar que comprueba si un elemento esta en alguna clase de equivalencia
elegido :: Eq a => a -> [[a]] -> Bool
elegido x ys = or $ map (elem x) ys 

-- Genera la relacion indicada por el enunciado empleando una lista intensional
--   Para cada elemento y entre 1 y m, se listan los elementos x entre n e y que dividen a ese Y
generaDiv :: Int -> Int -> Rel Int
generaDiv n m =  ( R [(x,y) | y <- [1..m], x <- [n..y] , mod y x == 0 ] )

-- Genera una relacion con el comparador >= empleando una lista intensional
generaGE:: (Ord a, Eq a) => [a] -> Rel a
generaGE xs = (R [(x,y) | x <- xs , y <- xs , x >= y])

-- Comprueba si ambas relaciones a componer son relaciones.
-- Si lo son, por cada par (t,t2) de la segunda relacion, se añaden a la composicion
--    todos los pares (t,y) donde y es un el elemento del par (t2,y) en la primera relacion
composicion:: Eq a => (Rel a) -> (Rel a) -> (Rel a)
composicion (R xs) (R ts) = if esRelacion (R xs) && esRelacion (R ts) then R (quitaRepetidos [(t,y) | (t,t2) <- ts , y <- relacionadosCon t2 xs] []) else R [] 

-- Definiciones de relaciones para probar

r1 :: Rel Int
r1 = R [(1,1),(2,2),(3,3),(1,2),(2,1),(3,1),(1,3),(3,2),(2,3)]

r2 :: Rel Int
r2 = R [(2,3),(3,2),(1,3),(3,1),(2,1),(1,2),(3,3),(2,2),(1,1)]

r3 :: Rel Char
r3 = generaGE ['a','b','c','d']

r4 :: Rel Char
r4 = R [('a','b'),('c','d'),('e','f'),('g','h')]

r5 :: Rel Char
r5 = R [('a','b'),('b','a'),('b','b'),('c','c'),('c','b'),('c','a'),('b','c'),('a','c'),('a','a')]

-- Ejercicio 4 ->  Funciones de E/S

introRel:: IO (Rel  Char)
introRel = (introAux (R []))

-- Funcion para pedir al usuario una relacion.
-- Pide los elementos del par uno por uno, y comprueba si el par ya se ha introducido
--   Si ese par se ha introducido previamente, simplemente lo ignora y pide el siguiente
--   Si el par no esta ya en la relacion, lo introduce y pide el siguiente
introAux:: (Rel Char) -> IO (Rel Char)
introAux (R xs) = do 
                 putStr "Introduce el un par de la relacion. Usa (.,.) para terminar \n"
                 n <- do 
                      putStr "Introduce el primer elemento del par \n"
                      leer <- getLine
                      return (read leer )
                 m <- do 
                      putStr "Introduce el segundo elemento del par \n"
                      leer2 <- getLine
                      return (read leer2)
                 if n == '.' && m == '.' then return (R xs) 
                                         else if elem (n,m) xs then (introAux (R xs)) else introAux (R ((n,m):xs)) 

-- Pide una relacion al usuario, y crea una representacion textual de ella con una matriz de adyacencia
-- Despues muestra la matriz por pantalla
muestraRel :: IO ()
muestraRel = do r <- introRel
                putStrLn (generaMatriz r)

-- Dada una relacion, genera una matriz de adyacencia
-- Genera la primera linea, y despues va generando para cada elemento la linea correspondiente
-- Se aprovecha que al emplear let para calcular una variable local, el calculo solo se hace una vez
generaMatriz ::(Eq a, Show a) => Rel a -> String
generaMatriz (R xs) = let elem = elementos (R xs) 
                          l1 = primeraLinea elem [] in 
                          "  | " ++ l1 ++ "\n" ++ 
                          "-----" ++ ([(\p -> '-') x | x <- [1..(length l1)]]) ++ "\n" ++
                          concat ( map (f.(generaLinea xs elem [])) elem ) where f x = x ++ "\n"

-- Funcion que calcula la primera linea de la matriz por recursion final
primeraLinea :: Show a => [a] -> String -> String
primeraLinea [] xs = xs
primeraLinea (x:xs) ys = primeraLinea xs (ys ++ " " ++ (show x))

-- Funcion para generar una fila de la matriz de adyacencia recursivamente
-- Va mirando los elementos de la relacion uno por uno en el orden en el que aparecen en la primera fila
--   Si estan relacionados con el elemento e, pone una X y si no pone espacios
--   Cuando ya ha mirado todos, pone por delante el indice de la matriz , la barra, y devuelve la fila creada
generaLinea:: (Show a, Eq a) => [(a,a)] -> [a] -> String -> a -> String
generaLinea rel [] xs e = (show e) ++ "|  " ++ xs
generaLinea rel (u:us) str e = if elem (e,u) rel then generaLinea rel us (str ++ "X   ") e else generaLinea rel us (str ++ "    ") e 

-- Pide una relacion al usuario y crea una representacion textual de ella con listas de adyacencia
muestraRelL :: IO()
muestraRelL = do r <- introRel
                 putStrLn (generaLAdyacencia r)

-- Genera una lista de adyacencia para cada elemento del conjunto soporte 
generaLAdyacencia:: (Show a, Eq a) => Rel a -> String
generaLAdyacencia (R xs) = concat ( map (f.(generaLista xs )) (soporte (R xs)) ) where f x = x ++ "\n"

-- Genera una lista de relacionados con el elemento e que se le pasa empleando una lista intensional
generaLista :: (Show a, Eq a) => [(a,a)] -> a -> String
generaLista xs e = take ((length s)-2) s where s =(show e) ++ " : " ++ concat [show(t) ++ ", " | t <- relacionadosCon e xs]