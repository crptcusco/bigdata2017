{-|
Module      : EXERCICIOS2
Description : Exercicio08
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

eleTrianguloPascal :: Integer -> Integer -> Integer
eleTrianguloPascal m n = (fatorial m-1) `div` ((fatorial n) *  fatorial ((m-n-1)))

main = do
--Declaracion de Variables
 let num1 = 4
 let num2 = 3

--Salida del Programa
 print (eleTrianguloPascal num1 num2)

