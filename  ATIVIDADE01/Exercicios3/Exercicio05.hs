{-|
Module      : EXERCICIOS3
Description : Exercicio05  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Control.Parallel

main :: IO()
--FUNCIONES
productoEscalar:: Double->Double->Double->Double->Double->Double
productoEscalar x1 y1 x2 y2 angulo = expr
 where
  expr = (normaVec x1 y1) * (normaVec x2 y2) * cos (angulo) 

normaVec :: Double-> Double -> Double   
normaVec x y = sqrt (x^2 +y^2) 

main = do
--ENTRADAS
 let x1 = 3
 let y1 = 0
 let x2 = 5
 let y2 = 5
 let angulo = pi/4
--SAIDA
--print (cos pi)
 print (productoEscalar x1 y1 x2 y2 angulo) 
 
