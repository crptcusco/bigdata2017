{-|
Module      : EXERCICIOS1
Description : Exercicio02  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
mult3 :: Integer -> Bool
mult3 x
 | x `mod` 3 == 0 = True    
 | otherwise = False    
 
main = do
--ENTRADAS
 let numero = 5
--SAIDA
 print (mult3 numero)
