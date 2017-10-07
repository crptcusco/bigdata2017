{-|
Module      : EXERCICIOS2
Description : Exercicio07
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
-- Toma un entero y devuelve si es o no primo

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

coeBinomial :: Integer -> Integer -> Integer
coeBinomial m n = (fatorial m) `div` ((fatorial n) *  fatorial (m-n))


main = do
--Declaracion de Variables
 let num1 = 3
 let num2 = 2

--Salida del Programa
 print (coeBinomial num1 num2)


