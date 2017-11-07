{-|
Module      : EXERCICIOS3
Description : Exercicio05  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
 
main :: IO()
--FUNCIONES
productoEscalar:: [Double]->[Double]->Double
productoEscalar [x] [y] = x*y
productoEscalar (x:xr) (y:yr) = x*y + productoEscalar xr yr

main = do
--ENTRADAS
let x = [3,4,5]
let y = [2,3,4]

--SAIDA
print (productoEscalar x y) 
