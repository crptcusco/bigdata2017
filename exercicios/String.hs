{-|
Module      : EXERCCIOS
Description : String
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
main = do
-- Concatenacion
 let s1 = "Ola "
 let s2 = ['m', 'u', 'n', 'd', 'o']
 
-- Conversion de tipos 
 let x = read "2" :: Integer
 let a = 4 :: Integer
 let y = read "2.3" :: Double
 let z = read "True" :: Bool
 let sx = show x
 let sy = show y
 let sz = show z 
 
 print(x + a)
 print(sz)
