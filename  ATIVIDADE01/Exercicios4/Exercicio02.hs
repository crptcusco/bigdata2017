{-|
Module      : EXERCICIOS4
Description : Exercicio01
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
sumarDiagonal :: [[Integer]]-> Int
sumarDiagonal matriz = longitud
 where
  longitud = length matriz   
 

main = do
--ENTRADAS
 let matriz = [[1,0,0],[0,1,0],[0,0,1]]

--SAIDA
 print (sumarDiagonal matriz)
