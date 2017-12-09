module Problema1 where

-- Función que suma los primeros n múltiplos de 3 y 5.
multiplos3Y5 :: Int -> Int
multiplos3Y5 0 = 0
multiplos3Y5 n
   | mod n 3 == 0 || mod n 5 == 0 = n + multiplos3Y5 (n - 1)
   | otherwise = multiplos3Y5 (n - 1)

-- Para encontrar la solución hay que encontrar los múltiplos entre 0 y 999 (menores a 1000).
solucion = multiplos3Y5 999
