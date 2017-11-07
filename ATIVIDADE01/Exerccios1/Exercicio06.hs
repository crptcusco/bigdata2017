{-|
Module      : EXERCICIOS1
Description : Exercicio06
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
div2d :: Integer -> Double
div2d x =  fromIntegral x / 2  
main = do
--ENTRADAS
 let numero = 5
--SAIDA
 print (div2d numero)
