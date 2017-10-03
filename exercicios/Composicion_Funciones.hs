{-|
Module      : EXERCCIOS
Description : Compisicion de Funciones
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
--Primer Ejemplo normal
mediaFinal :: Double -> Double -> Double
mediaFinal p1 p2 = 0.4*p1 + 0.6*p2
conceito :: Double -> Char
conceito media
 | media < 5 = 'F'
 | media < 6 = 'D'
 | media < 7 = 'C'
 | media < 8 = 'B'
 | otherwise = 'A'

geraConceito :: Double -> Double -> Char
geraConceito p1 p2 = conceito $ mediaFinal p1 p2

--Segundo Ejemplo con .
nota :: Double -> Double
nota x = x*2
conceito2 :: Double -> Char
conceito2 x
 | x > 5 = 'A'
 | otherwise = 'F'
calcConceito = conceito2 . nota
main = do
--Declaracion de Variables
 let a = 10.0
 let b = 5.0
 
--Salida del Programa
 print (geraConceito a b)
 print (calcConceito b)

 
