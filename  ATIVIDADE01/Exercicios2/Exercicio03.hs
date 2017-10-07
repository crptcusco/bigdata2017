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

multEtiope :: Integer -> Integer -> Integer ->Integer
multEtiope m n res
 | (m `div` 2) > 0  = multEtiope (m`div`2) (n*2) (res + (n*2))
 | m == 1 = res 
 
main = do
--Declaracion de Variables
 let num1 = 14
 let num2 = 12

--Salida del Programa
 print (multEtiope num1 num2 0)

