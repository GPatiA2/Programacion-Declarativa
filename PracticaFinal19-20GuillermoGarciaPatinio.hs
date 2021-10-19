-- Guillermo Garcia Patiño Lenza

data Vertice = A|B|C|D|E|F|H deriving (Read, Show, Eq)

data Grafo = G [Vertice] [(Vertice,Vertice)] deriving (Read, Show)

--- datos de ejemplo

g1,g2,g3 :: Grafo
g1 = G [B,D,E,C] [(D,E),(E,B),(C,B),(E,C)]
g2 = G [D,F,E] [(D,F),(E,D),(D,E),(F,E)]
g3 = G [A,C,D] [(A,C),(C,D),(A,D)]
g4 = G [A,B,C,D,E,F,H] [(A,D),(A,C),(A,B),(D,E),(C,F),(B,H)]
g5 = G [A,B,C,D,E,F,H] [(A,B),(B,C),(C,E),(E,F),(F,H),(H,D),(H,A),(D,A)]
---
-- Matriz de adyacencia
mat_ady :: Grafo -> [[Bool]]
mat_ady (G v a) = [ [existeArista v1 v2 a | v2 <- v ] | v1 <- v ]

-- Comprobar si existe una arista desde un vertice a otro
existeArista:: Vertice -> Vertice -> [(Vertice, Vertice)] -> Bool
existeArista v1 v2 [] = False
existeArista v1 v2 ((a,b):xs) = if (v1 == a) && (v2 == b) then True else  existeArista v1 v2 xs

-- Sacar los grados positivos (de salida)
grados_pos :: Grafo -> [Int]
grados_pos (G v a) = [grado v1 a True 0 | v1 <- v]

-- Sacar los grados negativos (de llegada
grados_neg :: Grafo -> [Int]
grados_neg (G v a) = [grado v1 a False 0 | v1 <- v]

-- Saca el grado de salida (True) o de entrada (False) de un vertice
grado:: Vertice -> [(Vertice,Vertice)] -> Bool -> Int -> Int
grado v [] _ g = g
grado v ((a,b):xs) True g = if (a == v) then grado v xs True (g+1) else grado v xs True g
grado v ((a,b):xs) False g = if (b == v) then grado v xs False (g+1) else grado v xs False g 

-- Obtener los vertices adyacentes a uno
ady :: Grafo -> Vertice -> [Vertice]
ady (G v a) v1 = [y | (x,y) <- a , (x == v1)]

-- Sacar todos los caminos de longitud n partiendo desde v1
camino_lng :: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng g v1 0 = [[v1]]
camino_lng g v1 n = filter (\xs -> xs /= []) [ concat (map f (camino_lng g v' (n-1))) | v' <- (ady g v1)] where f xs = v1:xs  

-- Comprobar si es conexo
-- Mirar si un vertice esta conectado con el resto, para todos los vertices
conexo:: Grafo -> Bool
conexo (G xv a) = foldr (&&) True [conectado v v1 (G xv a) [] | v1 <- xv , v <- xv , (\x -> x /= v1) v ] 

-- Comprueba si un vertice esta conectado a otro
conectado:: Vertice -> Vertice -> Grafo -> [Vertice] -> Bool
conectado v1 v2 (G v a) xv = existeArista v1 v2 a || any (\b -> b == True) [conectado v3 v2  (G v a) (v1:xv) | v3 <- ady (G v a) v1 , (\x -> not (elem x (v1:xv))) v3]