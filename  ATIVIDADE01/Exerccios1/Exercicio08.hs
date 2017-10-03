{-|
Module      : EXERCICIOS1
Description : Exercicio08
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
bissexto::Integer->Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

listaBiciestos n =  [ x | x <-[0..n] , bissexto x ]

main = do
--ENTRADAS
 let anio = 2017
--SAIDA
 print (listaBiciestos anio)
 
