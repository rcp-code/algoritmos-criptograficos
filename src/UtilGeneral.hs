module UtilGeneral where

    {----------------------------------------------------------------------
            Incluye algunas funciones auxiliares o de utilidad
            para distintos ficheros, de forma que queden todos
                    unificados en un mismo lugar.
    ----------------------------------------------------------------------}


import Tipos
import Constantes

import Data.Vector as V
import Data.Matrix as M
import Prelude as P
import Data.Char
import Data.List as L
import System.Random
import Data.Word
import Math.NumberTheory.Logarithms
import Data.Maybe
import GHC.Enum

    {----------------------------------------------------------------------
                            Comprobaciones
    ----------------------------------------------------------------------}

--Comprueba si la lista está vacía
esVacia :: [a] -> Bool
esVacia = L.null

--Comprueba si el primer elemento de una lista es 0
esPrimerElementoCero :: (Eq a, Num a) => [a] -> Bool
esPrimerElementoCero ls
    | primero ls==0 = True
    | otherwise = False

--Comprueba que una posición está en una lista dada
compruebaPosicionLista :: Int -> [a] -> Bool
compruebaPosicionLista pos lista
    | pos >= 0 && pos < L.length lista = True
    | otherwise = False

    {----------------------------------------------------------------------
                        Transformaciones de datos
    ----------------------------------------------------------------------}

--Transforma un número en una lista de sus dígitos
digitos :: Int -> [Int]
digitos n = [read [x] :: Int | x<-show n]

--Transforma una lista de dígitos en el número correspondiente
deDigitosANum :: (Integral a, Num a) => [a] -> a
deDigitosANum = L.foldl (\acc x -> acc * 10 + x) 0

--Transforma un número entero en su equivalente binario (lista)
cambioABase2 :: (Integral a, Num a) => a -> [a]
cambioABase2 num = cambioABase2' num []
    where
        cambioABase2' num listaRestos
            | num == 0 = 0:listaRestos
            | num == 1 = 1:listaRestos
            | otherwise = cambioABase2' (div num 2) (mod num 2:listaRestos)     --se añade el resto a la lista de los restos ya calculados  

--Transforma una lista de enteros en una lista de binario (lista de listas)
cambioABase2Lista :: (Integral a, Num a) => [a] -> [[a]]
cambioABase2Lista ns = [cambioABase2 x | x<-ns]

--Transforma una lista de binarios a su número correpondiente en base 10
deListaBinarioANum :: Num a => [a] -> a
deListaBinarioANum ns = abs $ L.sum [(2^p)*x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

    {----------------------------------------------------------------------
                        Tratamiento de datos
    ----------------------------------------------------------------------}

--Obtiene el primer elemento de una tupla de tres elementos
fst' :: (a, b, c) -> a
fst' (val1, val2, val3) = val1

--Obtiene el segundo elemento de una tupla de tres elementos
snd' :: (a, b, c) -> b
snd' (val1, val2, val3) = val2

--Obtiene el tercer elemento de una tupla de tres elementos
trd' :: (a, b, c) -> c
trd' (val1, val2, val3) = val3

--Obtiene el tercer elemento de una tupla de cuatro elementos
fst'' :: (a, b, c, d) -> a
fst'' (val1, val2, val3, val4) = val1

--Obtiene el tercer elemento de una tupla de cuatro elementos
snd'' :: (a, b, c, d) -> b
snd'' (val1, val2, val3, val4) = val2

--Obtiene el tercer elemento de una tupla de cuatro elementos
trd'' :: (a, b, c, d) -> c
trd'' (val1, val2, val3, val4) = val3

--Obtiene el cuarto elemento de una tupla de cuatro elementos
frt'' :: (a, b, c, d) -> d 
frt'' (val1, val2, val3, val4) = val4

--Intercambia los elementos de una tupla de dos elementos:
intercambia :: (a, b) -> (b, a)
intercambia (x,y) = (y,x)

--Obtiene el primer elemento de una lista
primero :: [a] -> a
primero = L.head

--Obtiene el último elemento de una lista
ultimo :: [a] -> a
ultimo = L.last

--Obtiene el elemento iésimo de una lista
obtieneElemento :: [a] -> Int -> a
obtieneElemento lista i = lista !! i

--Obtiene una sublista de una lista de listas
obtieneSubLista :: [[a]] -> Int -> [a]
obtieneSubLista listas i = listas !! i

--Obtiene la sublista a partir de dos posiciones, ambas posiciones incluidas
slicing :: [a] -> Int -> Int -> [a]
slicing [] _ _ = []
slicing lista inicio fin = slicing' lista inicio fin []

slicing' :: [a] -> Int -> Int -> [a] -> [a]
slicing' [] _ _ _ = []
slicing' lista inicio fin aux
    | fin>=inicio = slicing' lista inicio (fin - 1) (e:aux)
    | otherwise = aux
    where
        e = lista !! fin

--Número de dígitos de un número
numeroDigitos :: Int -> Int
numeroDigitos n = L.length $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = L.genericLength $ digitos n

--Obtiene el elemento central de una lista
elementoCentral :: [Int] -> Int
elementoCentral lista = lista !! pos
  where
    tamLista = L.length lista
    pos = abs $ div tamLista 2

--Obtiene el número de bits de un número entero
numeroBits :: Integer -> Int
numeroBits numero
    | numero>0 = succ $ integerLog2 numero
    | otherwise = error "El número es menor que 0."

--Comprueba si un caracter está dentro de la lista de caracteres
estaEnCaracteres :: Char -> Bool
estaEnCaracteres caracter = caracter `L.elem` caracteres

--Comprueba si un número está dentro de la lista de asociaciones
estaEnAsociaciones :: Int -> Bool
estaEnAsociaciones numero = numero `L.elem` numerosAsociados
    where
        numerosAsociados = L.map snd asociaciones
        
-- Obtiene el índice de un elemento de caracteres
obtieneIndice :: Char -> Int
obtieneIndice caracter = primero [i | (c,i)<-asociaciones, c==caracter]