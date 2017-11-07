{-|
Module      : EXERCICIOS3
Description : Exercicio08
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Control.Parallel

main :: IO()
--FUNCIONES
--genLista :: Integer -> [Integer] 
--genLista n =  map collatz [1..(collatz n-1)]
collatzMillon :: [Integer] -> Integer
collatzMillon (x:xr) = fst (collatzMillonAux (x , collatzLon x) xr)
  
collatzMillonAux :: (Integer,Integer) -> [Integer] -> (Integer, Integer)
collatzMillonAux (x,longitud) [] = (x,longitud)
collatzMillonAux (m,longitud) (l : lr)
  | collatzLon l > longitud = collatzMillonAux (l ,collatzLon l) lr
  | otherwise = collatzMillonAux (m,longitud) lr

collatzLon :: Integer -> Integer
collatzLon num = collatzLonAux num 1

collatzLonAux :: Integer -> Integer -> Integer
collatzLonAux num suma
  | num == 1 = suma
  | otherwise = collatzLonAux (collatz num) (suma+1)
  
collatz :: Integer->Integer
collatz x
 | x `mod` 2 == 0 =  x `div` 2
 | otherwise = (3* x)+1 

main = do
--ENTRADAS
 let num1 = 3 
--SAIDA
 print ( collatzMillon [1..1000000])
