{-|
Module      : EXERCCIOS
Description : Recursion_Factorial
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()

main = do
--Declaracion de Variables
 let lista1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 let lista2 = [1..10] -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 let lista3 = [0,2..10] -- [0, 2, 4, 6, 8, 10]
 let lista4 = [0,2..] -- [0, 2, 4, 6, 8, 10,..]
 
 let lista5 = [ 2*x | x <- [1..10] ]
 let lista6 = [ 2*x | x <- [0..] ]
 let lista7 = [x | x <- [1..100], x `mod` 5 == 0]
 let lista8 = [(x,y) | x <- [1..10], y <- [1..10]]
--Salida del Programa
 print (lista8)
