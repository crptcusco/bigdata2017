{-|
Module      : EXERCICIOS1
Description : Exercicio12
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}
import Data.Char

main :: IO()
--FUNCIONES
concatenador::String->String->String
concatenador x y = x ++ " " ++ y



main = do
--ENTRADAS
 let palabra1 = "0123456789"
 let palabra2 = "dia"
 -- let lista = ( map ( :[] ) palabra1)
 -- let lista = map ( \c -> [c] ) palabra1 
 let lista2 = map (digitToInt) palabra1
--SAIDA
 print ( lista2 )
 print ( map (+4) lista2 )
