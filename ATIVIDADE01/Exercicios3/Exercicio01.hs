{-|
Module      : EXERCICIOS3
Description : Exercicio01  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Control.Parallel

main :: IO()
--FUNCIONES
--divisivel20 :: Integer -> Bool  
--divisivel20 num = expr
 
divisible20 :: Integer -> Bool
divisible20 num = expr a b
 where
  a = num
  b = [1..20]
expr :: Integer -> [Integer]-> Bool
expr a [] = True
expr a (x:lista) = (divisible a x) && (expr a lista)       
  
divisible :: Integer -> Integer -> Bool
divisible a b
 | a `div` b == 0 = True
 | otherwise = False

multipli :: [Integer] -> Integer
multipli (x:xs) = x * multipli     
multipli [] = 1

main = do
--ENTRADAS
 let num = 40

--SAIDA
 print ( divisible20 num )
 