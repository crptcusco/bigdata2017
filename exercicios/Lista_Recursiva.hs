{-|
Module      : EXERCCIOS
Description : Lista Recursiva
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
lista1 = 1 : prox lista1
 where
  prox (x:resto) = (x+1) : prox resto
main = do
--Declaracion de Variables
 
--Salida del Programa
 print (take 5 (lista1))
