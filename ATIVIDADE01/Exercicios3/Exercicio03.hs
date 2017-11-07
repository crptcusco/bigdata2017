{-|
Module      : EXERCICIOS3
Description : Exercicio03  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Control.Parallel

main :: IO()
--FUNCIONES

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = n1 `par` (n1 + n2)
 where
  n1 = fib (n - 1)
  n2 = fib (n - 2)

geraFib :: Integer -> [Integer]
geraFib 0 = [1]
geraFib 1 = [1,1]
geraFib n = (fib n):(geraFib(n -1))

fibs1 ::Integer -> [Integer]
fibs1 n = [fib n | n <- [0..n]]

main = do
--ENTRADAS
 let num = 10
 
 
--SAIDA
 print (geraFib num)
 print (fibs1 10)
 
