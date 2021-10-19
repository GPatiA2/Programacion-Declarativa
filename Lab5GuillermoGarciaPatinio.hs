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

-- lineas <- lines contenido
-- justificado <- map (justificar l) lineas 
-- escribir <- unlines justificado
-- formatea fileIn fileOut l = fileWrite (unlines (transforma (lines (readFile fileIn) l)))