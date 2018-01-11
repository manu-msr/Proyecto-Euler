module Problema4 where

import Data.List

-- Función que verifica si un número es palíndromo.
esPalindromo :: Int -> Bool
esPalindromo x = show x == reverse (show x)

-- Función que encuentra el primer número palíndromo en una lista de números.
verifica :: [Int] -> Int
verifica (x:xs)
   | esPalindromo x = x
   | otherwise = verifica xs

-- Se generan todos los posibles productos entre números de tres dígitos. Se ordena la lista y se
-- busca el primer número palíndromo.
solucion = verifica (reverse (sort [x*y | x <- [999,998..100], y <- [999,998..100]]))
