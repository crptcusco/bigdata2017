{-|
Module      : EXERCICIOS1
Description : Exercicio05
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
mult2pos :: Integer -> Bool
mult2pos x
 | x < -1 || (x>1 && x `mod` 2 == 0) = True    
 | otherwise = False    

main = do
--ENTRADAS
 let numero = 0
--SAIDA
 print (mult2pos numero)
