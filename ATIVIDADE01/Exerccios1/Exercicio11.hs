{-|
Module      : EXERCICIOS1
Description : Exercicio11
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
concatenador::String->String->String
concatenador x y = x ++ " " ++ y

main = do
--ENTRADAS
 let palabra1 = "bom"
 let palabra2 = "dia"
--SAIDA
 print ( concatenador palabra1 palabra2)
