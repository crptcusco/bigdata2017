{-|
Module      : EXERCICIOS2
Description : Exercicio04
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--Declaracion de Funciones
-- Toma un entero y devuelve si es o no primo

lcand n = [2..(n - 1)]

tem_divisor n [ ] = False
tem_divisor n (x:xs) = if (mod n x) == 0 then True else tem_divisor n xs

primo n = not (tem_divisor n (lcand n))

main = do
--Declaracion de Variables
 let num = 30

--Salida del Programa
 print (primo num)
