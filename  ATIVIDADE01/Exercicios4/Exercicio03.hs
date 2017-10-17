{-|
Module      : EXERCICIOS4
Description : Exercicio03
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
sumarDiagonal matriz = foldr (+) 0 (listaDiagonal matriz (toInteger (length matriz -1)))

listaDiagonal :: [[Integer]]-> Integer -> [Integer]
listaDiagonal matriz pos
  | pos < 0 = []
  | pos >= 0 = (matriz !!0 !! (fromIntegral pos)):(listaDiagonal(tail matriz) (pos-1))

main = do
--ENTRADAS
 let matriz1 = [[8,0,0],[0,1,0],[0,0,1]]
 let matriz2 = [[0,0,0,1],[0,0,1,0],[0,2,0,0],[1,0,0,0]]

--SAIDA
 print (listaDiagonal matriz1 (fromIntegral (length matriz1 -1)))
 print (sumarDiagonal matriz1)
 print (listaDiagonal matriz2 (fromIntegral (length matriz2 -1)))
 print (sumarDiagonal matriz2)
