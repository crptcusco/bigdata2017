{-|
Module      : EXERCCIOS
Description : Patter_Matching
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
mult :: (Eq a, Num a) => a -> a -> a
mult 1 y = y
mult x 1 = x
mult 0 _ = 0
mult _ 0 = 0
mult x y = x * y

main = do
--Declaracion de Variables
 let a = 1
 let b = 0
 
--Salida del Programa
 print(mult a b)
