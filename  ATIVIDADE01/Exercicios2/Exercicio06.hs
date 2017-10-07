{-|
Module      : EXERCICIOS2
Description : Exercicio06
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
-- Toma un entero y devuelve si es o no primo

persisAditive :: Integer -> Integer
persisAditive n
  | n >= 10 = persisAditive (sumaDigitos(n))  
  | otherwise = sumaDigitos(n)

sumaDigitos :: Integer->Integer
sumaDigitos a
  | a `div` 10 >=1 = sumaDigitos (a `div` 10) + (a `mod` 10)    
  | otherwise = a

main = do
--Declaracion de Variables
 let num = 99

--Salida del Programa
 print (persisAditive num)
