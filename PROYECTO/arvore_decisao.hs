{-|
Module      : Proyecto
Description : Arbol de desicao
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com
Induce y deduce modelos de clasificacion
-}

module Main where

import System.IO
--import Math.Statistics
import System.Environment
import Control.Parallel.Strategies
import Data.List (concat)
import Data.List.Split (chunksOf)

import Data.List
import Data.Ord (comparing)

-- METODOS
generarTabla:: String -> [[String]]
generarTabla file = tail (map parseLine (lines file))
 where
  parseLine l =  tail(words l)

generarData:: [[String]] -> [[String]]
generarData datatable = transpose datatable
     
     
generarTablaAtributos :: String -> [String]
generarTablaAtributos file = (map parseLine (lines file))!!0
 where
  parseLine l = (tail(words l))

-- generarTablaTipos :: [String]->[[String]]->[(String,[String])]
-- generarTablaTipos cabecera dataset = map recorrerLinea (cabecera,dataset)
-- where
--  recorrerLinea = cabecera
  
generarAtributosNominales :: [String]->[[String]] -> [(String,[String])] 
generarAtributosNominales cabecera dataTranspuesta = crearColumna cabecera dataTranspuesta
 where
  crearColumna :: [String] ->[[String]] -> [(String,[String])]
  crearColumna cabecera dataTranspuesta 
   | cabecera == [] = []
   | otherwise = ( cabecera !!0 , unicos (dataTranspuesta !! 0)) : (crearColumna (tail cabecera) (tail dataTranspuesta))

innitNominales:: [(String,[String])] ->[Bool]
innitNominales [] = []
innitNominales (x:xr)
 | xr==[] =[True]
 |otherwise = (False: innitNominales xr)

unicos:: [String]-> [String]
unicos columnaTranspuesta = nub columnaTranspuesta

filtarLineas :: [[String]] -> Int -> String -> [[String]]
filtarLineas atributosNominales columna criterio = iterarLineas atributosNominales columna criterio
 where
  iterarLineas :: [[String]] -> Int -> String -> [[String]] 
  iterarLineas atributosNominales columna criterio 
   | atributosNominales == [[]] = []
   | ((atributosNominales !!0) !!columna) == criterio = (atributosNominales !!0):(iterarLineas (tail atributosNominales) columna criterio)       
   | ((atributosNominales !!0) !!columna) /= criterio = (iterarLineas (tail atributosNominales) columna criterio)
 
----ENTROPIA
-- datos classValues numeroValues 
entropy :: [[String]] ->[String]->Double->Double
entropy dataTable [] _  = 0.0
entropy dataTable (x:xr) nValues
 |prob==0 = 0
 |otherwise = -prob * (logBase nValues prob) + (entropy dataTable xr nValues)
 where
  prob = fromIntegral(length $ filterTable dataTable nColumns x)/nData
  nColumns=(length$dataTable!!0)-1
  nData =fromIntegral(length dataTable)
 
-- probab ::[[String]] ->String ->Double
-- probab datos elemento = fromIntegral(length $ filtro)/nData
 -- where
  -- filtro=filterTable datos nColumns elemento
  -- nColumns=(length$datos!!0)-1
  -- nData =fromIntegral(length datos)
filterTable :: [[String]] -> Int ->String ->[[String]]
filterTable datos iColumn value = concat lists
 where
 -- PARALELIZACION
  lists = map (filterTable' iColumn value) chunks `using` parList rdeepseq
  chunks = chunksOf 1000 datos
 

filterTable' :: Int ->String ->[[String]] ->[[String]]
filterTable' _ _ []= []
filterTable' iColumn value (x:xr)
 | x!!iColumn ==value = x:(filterTable' iColumn value xr)
 | otherwise = filterTable' iColumn value xr

--datos classValues attValues
entropyAttr :: [[String]] ->[String] ->[String] ->Int ->Double
entropyAttr _ _ [] _ = 0
entropyAttr datos classValues (x:xr) posAtt = lengtList/nDatos * (entropy listFiltered classValues nValues) + (entropyAttr datos classValues xr posAtt)
 where
  nDatos=fromIntegral$length(datos)
  nValues=fromIntegral$length(classValues)
  listFiltered = filterTable datos posAtt x
  lengtList =fromIntegral$length(listFiltered)

mutualInformation :: [[String]] ->[String] ->Int ->[String] ->Double
mutualInformation datos classValues nColumn attValues = entropyPriory - entropyAtt
 where
  entropyPriory = entropy datos classValues nClass
  nClass =fromIntegral$length classValues
  entropyAtt = entropyAttr datos classValues attValues nColumn


-- arbol de decision

data Tree a = EmptyNode
              | Node a [(Tree a)]
              deriving (Show)
-- datos attNominales isProcess valueAttrPadre -> Tree(valAttribPadre, idAttr,valClass)

crearArbol::[[String]] -> [(String,[String])] -> [Bool] -> String ->Tree (String,Int,String)
crearArbol [] _ _  valAttrib = Node (valAttrib, -1,"") []
crearArbol datos attNominales isProcessAtrib valAttrib
 | and isProcessAtrib ==True = Node (valAttrib,-1, snd$definirClase datos) [] -- no quedan atributos
 | fst$definirClase datos = Node (valAttrib,-1, snd$definirClase datos) [] -- todos los datos son de una misma clase
 | otherwise = Node (valAttrib, fst$(nodeAtribute),"") (snd$(nodeAtribute))
  where
   nodeAtribute = definirNodo datos attNominales isProcessAtrib


definirClase:: [[String]] ->(Bool,String)
definirClase datos
 | (fst$modeClasses) == length(classes) = (True, snd$modeClasses)
 | otherwise = (False, snd$modeClasses)
   where
    classes = map last datos
    modeClasses = head $ modes $ classes
    
modes :: (Ord a) => [a] -> [(Int, a)]
modes xs = sortBy (comparing $ negate.fst) $ map (\x->(length x, head x)) $ (group.sort) xs

definirNodo:: [[String]] -> [(String,[String])] -> [Bool] -> (Int,[Tree(String,Int,String)])
definirNodo datos attNominales isProcessAtrib = (posMax, listaPartes)
 where
  posMax=posMaxInformation datos attNominales isProcessAtrib 0 0 0
  listaPartes = dividirList datos attNominales isProcessAtrib (snd$attNominales!!posMax) posMax

dividirList:: [[String]] -> [(String,[String])] -> [Bool] -> [String]-> Int ->[Tree(String,Int,String)]
dividirList _ _ _[] _ = []
dividirList datos attNominales isProcessAtrib (valAtt:restoAtt) posAtt = arbol :(dividirList datos attNominales isProcessAtrib' restoAtt posAtt)
 where 
  arbol=(crearArbol (filterTable datos posAtt valAtt) attNominales isProcessAtrib' valAtt)
  isProcessAtrib' =take posAtt isProcessAtrib ++ [True] ++ drop (posAtt + 1) isProcessAtrib

posMaxInformation::[[String]] -> [(String,[String])] -> [Bool]->Double ->Int ->Int->Int
posMaxInformation datos [] [] _ _ maxPos = maxPos
posMaxInformation datos (att:restoAtt) (isPr:restoIsP) actInf actPos maxPos
 | isPr = posMaxInformation datos restoAtt restoIsP actInf (actPos+1) maxPos
 | (mi >= actInf) = posMaxInformation datos restoAtt restoIsP mi (actPos+1) actPos
 | otherwise = posMaxInformation datos restoAtt restoIsP actInf (actPos+1) maxPos
  where
    mi=mutualInformation datos (snd$last$restoAtt) actPos (snd$att) 
    
testData:: Tree(String, Int,String)->[String]->String
testData (Node (valAttr,idAtt,clase) hijos) test 
 | clase /= "" = clase
 | otherwise = testData (buscarHijos hijos (test!!idAtt)) test

buscarHijos:: [Tree(String, Int,String)] ->String ->Tree(String, Int,String)
--buscarHijos _ _ =Node("",0,"") []
buscarHijos (t@(Node (val,_,_) _): xr) valAttr
 | val==valAttr = t
 |otherwise = buscarHijos xr valAttr
-- EJECUCION
main :: IO ()
main = do
 file <- readFile ("data.txt")
 let dataTable = generarTabla file
 let cabecera = generarTablaAtributos file
 let dataTranspuesta = generarData dataTable
 let atributosNominales = generarAtributosNominales cabecera dataTranspuesta
--let cantidadRegistros = length dataset
 --print (dataTranspuesta)
 --print (cabecera)
 let pos=1
 let classValues =snd$last$atributosNominales
 let attValues=snd$atributosNominales!!pos
 --print (dataTable)
 print ("##### VALORES NOMINALES")
 print (attValues)
 print (atributosNominales)
 --print (filterTable dataTable pos (attValues!!0))
 --print (entropy dataTable classValues (fromIntegral$length classValues))
 --print (mutualInformation dataTable classValues pos attValues)
 --print (probab dataTable (classValues!!0))
 --let classes=map last dataTable
 --print (modes classes)
 --print (definirClase dataTable)
 let boolinit=innitNominales$atributosNominales
 print( boolinit)
 let arbol=crearArbol dataTable atributosNominales boolinit ""
 print ("### PRINT DO ARVORE - DEDUCCION")
 print(arbol)
 print ("### TEST DE PROBA - INFERENCIA")
 print ( testData arbol ["acuarela","brasil","moderno","no"])
 --print (testData arbol []
 --[[String]] -> [(String,[String])] -> [Bool] -> String ->Tree (String,Int,String)
