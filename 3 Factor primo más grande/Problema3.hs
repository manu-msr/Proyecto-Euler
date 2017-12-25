module Problema3 where

-- Función que encuentra los factores primos de un número.
factoresPrimos :: Int -> Int -> [Int] -> [Int]
factoresPrimos 1 _ primos = primos
factoresPrimos n p primos
   | mod n p == 0 = factoresPrimos (div n p) p (p:primos)
   | otherwise = factoresPrimos n (encuentraPrimo (p + 1)) primos

-- Función que dado un número, genera el siguiente número primo cercano.
encuentraPrimo :: Int -> Int
encuentraPrimo x
   | esPrimo x 2 = x
   | otherwise = encuentraPrimo (x + 1)

-- Función que indica si un número es primo. Usa un acumulador para verificar.
esPrimo :: Int -> Int -> Bool
esPrimo x y
   | x == y = True
   | mod x y == 0 = False
   | otherwise = esPrimo x (y + 1)

-- Para encontrar la solución hay que encontrar el mayor factor primo. Para esto se encuentran todos
-- los factores primos del número y luego se extrae el primer elemento de la lista.
solucion = head (factoresPrimos 600851475143 2 [])