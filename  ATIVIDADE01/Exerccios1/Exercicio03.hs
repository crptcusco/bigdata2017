{-|
Module      : EXERCICIOS1
Description : Exercicio03  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
mult5 :: Integer -> Bool
mult5 x
 | x `mod` 5 == 0 = True    
 | otherwise = False    
 
main = do
--ENTRADAS
 let numero = 5
--SAIDA
 print (mult5 numero)
