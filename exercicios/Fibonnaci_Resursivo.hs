{-|
Module      : EXERCCIOS
Description : Fibbonaci_Recursivo  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()

fib = 1 : 1 : 2 : prox fib
 where
  prox (x : t@(y:_)) = (x+y) : prox t

main = do
--Declaracion de Variables
 
--Salida del Programa
 print ( take 10 fib)

