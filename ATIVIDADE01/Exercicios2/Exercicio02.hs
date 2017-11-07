{-|
Module      : EXERCICIOS2
Description : Exercicio02
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
tipoTriangulo :: Double -> Double -> Double -> String
tipoTriangulo d1 d2 d3
 | d1 == d2 && d2 == d3 = "Equilatero" 
 | d1 == d2 && d2 /= d3 || d1 == d3 && d2 /= d3 || d3 == d2 && d1 /= d3 = "Isoceles"
 | otherwise = "Escaleno"
main = do
--Declaracion de Variables
 let d1 = 2
 let d2 = 1
 let d3 = 3

--Salida del Programa
 print (tipoTriangulo d1 d2 d3)
