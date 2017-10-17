{-|
Module      : EXERCICIOS4
Description : Exercicio01
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
-}

main :: IO()
--FUNCIONES
generarMatriz :: Integer -> Integer-> [[Integer]]
generarMatriz filas acumulador
 | acumulador==1 =  [generarLista filas 1]    
 | filas >1 = generarMatriz filas (acumulador-1) ++  ([generarLista filas acumulador] )

generarLista longLista posUno = llenarLista longLista posUno

llenarLista :: Integer -> Integer -> [Integer]
llenarLista longLista posUno
 | longLista == posUno = 1:llenarLista (longLista-1) posUno
 | longLista == 0 = []
 | otherwise = 0:llenarLista (longLista-1) posUno

main = do
--ENTRADAS
 let num = 5

--SAIDA
 print (generarMatriz num num)
