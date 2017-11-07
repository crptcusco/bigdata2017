{-|
Module      : EXERCICIOS4
Description : Exercicio02
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
mitadLista lista
 |length(lista) == 1 = 1  
 |length(lista) > 1 = length(lista) `div` 2 

sumarDiagonal :: [[Integer]]-> Integer
sumarDiagonal matriz = foldr (+) 0 (listaDiagonal matriz (toInteger 0))

listaDiagonal :: [[Integer]]-> Integer -> [Integer]
listaDiagonal matriz pos
  | length matriz == 0 = []
  | length matriz > 0 = (matriz !!0 !! (fromIntegral pos)):(listaDiagonal(tail matriz ) (pos +1))

main = do
--ENTRADAS
 let matriz1 = [[1,0,0],[0,1,0],[1,0,2]]
 let matriz2 = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

--SAIDA
 print (listaDiagonal matriz1 0)
 print (sumarDiagonal matriz1)
 print (listaDiagonal matriz2 0)
 print (sumarDiagonal matriz2)
