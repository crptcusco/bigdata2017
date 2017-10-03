{-|
Module      : EXERCCIOS
Description : Recursivo 1
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO() 
main = do
 let zplus = [1..100]
 print (take 50 [2*i | i <- zplus])
