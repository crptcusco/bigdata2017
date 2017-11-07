{-|
Module      : EXERCICIOS3
Description : Exercicio06
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Control.Parallel

main :: IO()
--FUNCIONES

collatz :: Integer->Double
collatz x
 | x `mod` 2 == 0 = (fromIntegral x)/2
 | otherwise = (3* (fromIntegral x))+1 
main = do
--ENTRADAS
 let num1 = 3

--SAIDA
--print (cos pi)
 print (collatz num1)

