{-|
Module      : EXERCCIOS
Description : Anio Bisiesto
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))
main = do
 let anio = 1999
 print ( bissexto anio)
