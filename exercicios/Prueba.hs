{-|
Module      : EXERCICIOS1
Description : Exercicio03  
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
mult35 :: Integer -> Bool
mult35 x
 | x `mod` 5  == 0 && x `mod` 3 == 0 = True    
 | otherwise = False    
 
main = do
--ENTRADAS
 let numero = 18
--SAIDA
 print (mult35 numero)
