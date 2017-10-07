{-|
Module      : EXERCICIOS2
Description : Exercicio01
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
ehTriangulo :: Double -> Double -> Double -> Double -> Double -> Double -> Bool 
ehTriangulo x1 y1 x2 y2 x3 y3
  | (x3 - y1)/(x3 - x1) == (y1-y2)/(x1-x2) = False    
  | otherwise = True

main = do
--Declaracion de Variables
 let x1 = 0
 let y1 = 0
 let x2 = 5
 let y2 = 5 
 let x3 = 10
 let y3 = 11
 
--Salida del Programa
 print(ehTriangulo x1 y1 x2 y2 x3 y3)
