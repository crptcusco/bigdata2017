{-|
Module      : Proyecto
Description : Clasificacion de Atributos
Copyright   : (c) Carlos Portocarrero, 2017
License     : GPL-3
Maintainer  : crptcusco@gmail.com

Induce y deduce modelos de clasificacion
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies

import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl', nub, tails, intercalate)
import Data.Hashable (Hashable)

import Control.Parallel
import Control.Monad
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl')
import Data.List.Split (splitOn, chunksOf)
import Data.Hashable (Hashable)



-- METODOS

-- Leyendo el contenido y conviritiendo en Arreglo de Doubles
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
 where
  parseLine l = map toDouble (words l)
  toDouble  w = read w :: Double

parseFile2 :: String -> [[String]]
parseFile2 file = map parseLine (lines file)
 where
  parseLine l = words l

generarTabla:: String -> [(String,[String],String)]
generarTabla file = map parseLine (lines file)
 where
  parseLine l = ((words l)!! 0, tail(init(words l)), last (words l))
	
-- entropiaFila 

type Op a = (a -> a -> a)    -- | Function of two inputs
type Mapper k a = k -> (k,a) -- | Function that maps a key to a key value tuple

-- |'combiner' combines a list of tuples to a HashMap using 
-- a specified operator
combiner :: (Hashable k0, Eq k0)
         => Op a0 
         -> [(k0,a0)] 
         -> M.HashMap k0 a0
combiner f m   = M.fromListWith f m

-- | 'mapper' maps a list of keys to a HashMap of key-values 
-- using a mapper function and combining with an operator
mapper :: (Hashable k0, Eq k0) 
       => Op a0 
       -> Mapper k0 a0 
       -> [k0] 
       -> M.HashMap k0 a0
mapper combfun f job = combiner combfun $ map f job

-- |'reducer' reduces the output of the 'mapper' 
-- to produce the desired output
reducer :: (Hashable k0, Eq k0) 
        => Op a0 
        -> [M.HashMap k0 a0] 
        -> M.HashMap k0 a0
reducer f m  = foldl' (M.unionWith f) M.empty m


-- | 'preText' preprocess a text using MapReduce to run in parallel
preText :: Int -> String -> [String]
preText n text = concat words 
  where
    lowerReplace c = if isAlphaNum c then toLower c else ' '
    words = map procSent workers `using` parList rdeepseq
    procSent s = map (map lowerReplace) s
    sentences = splitOn " " text
    workers   = chunksOf n sentences

-- | 'wordcount' returns the frequency of each unique word
-- of a document using MapReduce to run in parallel
wordCount :: Int -> [String] -> M.HashMap String Integer
wordCount n wl = reducer (+) mapped
  where
    mapped     = map mapfun workers `using` parList rdeepseq
    mapfun     = mapper (+) counter
    counter w  = (w,1)
    workers    = chunksOf n wl


-- Transformar a columnas el dataset
-- transColumnas :: [[String]] -> [[String]]
-- transColumnas dataset = map voltearFila (dataset)
-- where
--  voltear fila f =  
	
extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

-- EJECUCION
main :: IO ()
main = do
 file <- readFile ("data.txt")
 let dataset = generarTabla file
 let cantidadRegistros = length dataset
 let cantidadPalabras =  wordCount 4 ( file )  
 print (dataset)
 print cantidadPalabras
    
