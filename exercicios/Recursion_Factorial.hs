{-|
Module      : EXERCCIOS
Description : Recursion_Factorial
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
--Primer Ejemplo normal
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)
main = do

--Declaracion de Variables
 let n = 5
 
--Salida del Programa
 print (fatorial n)

 
