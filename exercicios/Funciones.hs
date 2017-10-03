{-|
Module      : EXERCCIOS
Description : Funciones1
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
raiz ::  Double -> Double -> Double -> (Double,Double)
raiz a b c 
 | delta <0 = error "Raizes negativas"
 | otherwise = (x1,x2) 
 where
  x1 = (-b - sqrt delta) / (2*a)
  x2 = (-b + sqrt delta) / (2*a)
  delta = b**2 - 4*a*c

main = do
--Declaracion de Variables
 let a = 1
 let b = 7
 let c = 12
 
--Salida del Programa
 print(raiz a b c)
