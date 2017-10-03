{-|
Module      : EXERCCIOS
Description : Listas
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
main = do
 let lista1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 let lista2 = [1..10]
 let lista3 = [2,4..10]
 let lista4 = [2*i | i <- [1..5]]
 
 print (head lista1)
 print (tail lista3)
 print (lista2 !! 1)
 print (take 2 lista4)
 print (drop 2 lista4)
 print (lista3 ++ lista4)
 print (9 : lista3 ++ lista4)
