{-|
Module      : EXERCCIOS
Description : SOMA
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
soma :: Integer -> Integer -> Integer
soma x y = x + y
main = do
 let a = 10
 let b = 200
 print ( soma a b)
