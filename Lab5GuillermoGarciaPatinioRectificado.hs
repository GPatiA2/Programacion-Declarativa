--Guillermo Garcia Patinio Lenza

-- ej 1

adivina :: Int -> IO()
adivina n = do
-- Se pide un numero 
   putStr "Escribe el numero que hay que adivinar : "
   num <- do
-- Se lee una linea 
          leer <- getLine
-- Se devuelve 
          return (read leer :: Int)
   if num == n then putStr "Adivinaste el numero \n "
   else if num > n then do putStr " El numero que estas adivinando es menor que el que acabas de introducir \n"
                           adivina n        
        else do putStr " El numero que estas adivinando es mayor que el que acabas de introducir \n"
                adivina n

-- ej 2
-- Funcion del enunciado -> Lee un fichero, lo formatea, y lo escribe en otro
-- !! TODO LO QUE HAGA A LA DERECHA DE UNA FLECHA TIENE QUE SER I/O
formatea :: String -> String -> Int -> IO()
formatea fileIn fileOut l = do 
   contenido <- readFile fileIn
   writeFile fileOut (formatear l contenido)

-- Funcion que formatea el contenido del fichero
formatear :: Int -> String -> String
formatear l strIn = unlines ( map (justificar l) (lines strIn) ) 

-- Funcion que formatea una linea
justificar :: Int -> String -> String
justificar l xs = if length(words xs) == 1 then xs
                  else if length xs >= l then xs
                       else concat $ map (meterEspacios m) (words xs) where m = div (l - (sum (map length (words xs)))) ((length $ words xs) - 1)

meterEspacios :: Int -> String -> String
meterEspacios n p = p ++ [(\x -> ' ') n | n <- [1..n] ]

-- ej3

type Matriz a = [[a]] 

dimension :: Matriz a -> (Int, Int)
dimension (x:xs) = (a,b) where a = length (x:xs)
                               b = length x


transp :: Matriz a -> Matriz a
transp ([]:_) = []
-- Una lista con los primeros de cada fila (primera columna) como primera fila
-- Y eso lo pego a la transpuesta de lo siguiente
transp xs =  (map head xs) : (transp  (map tail xs)) 

-- Como la matriz es una lista de listas, si hago zipWith (zipWith (+)) , consigo
--  combinar las dos matrices en una, de manera que cada elemento de la nueva matriz es
--   zipWith (+) x y donde x e y son filas de la matriz
-- El resultado de combinar esas dos filas es una fila de la matriz resultante en la que
--  el primer elemento de la fila es la suma de los primeros elementos de la fila correspondiente
--  en cada matriz
sumaMat :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMat (x:xs) (y:ys) = if (dimension (x:xs)) /= (dimension (y:ys)) then [[]]
                        else zipWith (zipWith (+)) (x:xs) (y:ys)



-- Funcion que hace el producto de dos matrices comprobando sus dimensiones
-- Hace lo siguiente: Como ( a b ) * ( e f ) =  ( ae+bg  af+bh ) = ( ae af ) + ( bg bh )
--                         ( c d )   ( g h )    ( ce+dg  cf+dh )   ( ce cf )   ( dg dh )
-- Y eso se descompone en ( a ) * ( e f ) + ( b ) * ( g h ) 
--                        ( c )             ( d )
-- Entonces tengo que ir cogiendo pares (columna , fila) en orden, multiplicarlos
--   y sumarlos con lo que me de la multiplicacion de lo que queda
--
-- Un ejemplo con una (3,3) sería
--     ( a b c ) * ( j k l ) = ( a ) * ( j k l ) + ( b c ) * ( m n o ) 
--     ( d e f )   ( m n o )   ( d )               ( e f )   ( p q r ) 
--     ( g h i )   ( p q r )   ( g )               ( h i )   
--  El primer sumando es caso base y lo resuelvo con multSimple, y el segundo es caso recursivo

prodMat :: Num a => Matriz a -> Matriz a -> Matriz a
prodMat m1 m2 = if (b == c) && (c /= 1) then sumaMat ( multSimple (map (\x -> (head x):[]) m1) ((head m2):[]) )  ( prodMat (map tail m1) (tail m2)  )
                else if (b == c) && (c == 1) then multSimple m1 m2
                     else [[]]
                where b = snd $ dimension m1
                      c = fst $ dimension m2

-- Funcion que multiplica matrices de dimensiones (a,1) (1,b) y produce una (a,b)
-- Ejemplo [[a],[b],[c]] * [[d,e,f]] = [ [ad, ae, af] , [bd, be, bf] , [cd, ce, cf] ]
-- De una  (3*1) y una (1*3) consigue una (3*3)
multSimple :: Num a => Matriz a -> Matriz a -> Matriz a
multSimple ms1 ms2 = [[ x * (head y) | x <- (head ms2)] | y <- ms1 ]

dibujaMatriz :: Show a => Matriz a -> IO ()
dibujaMatriz m = putStr $ ordena m

ordena :: Show a => Matriz a -> String
ordena m = unlines [ concat (map (\n -> n ++ " ") $ map show x) | x <- m]