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

-- Comprueba si la lista está vacía
esVacia :: [a] -> Bool
esVacia = L.null

-- Comprueba si un número es entero
esEntero :: (RealFrac a) => a -> Bool
esEntero x = snd (properFraction x) == 0

esPrimerElementoCero :: (Eq a, Num a) => [a] -> Bool
esPrimerElementoCero ls
    | primero ls==0 = True
    | otherwise = False

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

--Transforma una lista de dígitos en el número que corresponde
-- deDigitosANum :: (Integral a, Num a) => [a] -> a
-- deDigitosANum ns = deDigitosANum' (L.reverse ns)
--     where
--         deDigitosANum' [x] = x
--         deDigitosANum' (x:xs) = x+10*deDigitosANum' xs

deDigitosANum :: (Integral a, Num a) => [a] -> a
deDigitosANum = L.foldl (\acc x -> acc * 10 + x) 0

cambioABase2 :: (Integral a, Num a) => a -> [a]
cambioABase2 num = cambioABase2' num []
    where
        cambioABase2' num listaRestos
            | num == 0 = 0:listaRestos
            | num == 1 = 1:listaRestos
            | otherwise = cambioABase2' (div num 2) (mod num 2:listaRestos)     --se añade el resto a la lista de los restos ya calculados  

cambioABase2Lista :: (Integral a, Num a) => [a] -> [[a]]
cambioABase2Lista ns = [cambioABase2 x | x<-ns]

deListaBinarioANum :: Num a => [a] -> a
deListaBinarioANum ns = abs $ L.sum [(2^p)*x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

-- Convierte una lista de bits a un byte
-- bitsAByte :: [Bool] -> Int
-- bitsAByte = L.foldl (\acc b -> acc * 2 + fromEnum b) 0

-- -- Convierte una lista de bits a un número entero
-- deBitsAInt :: [Bool] -> Int
-- deBitsAInt = L.foldl (\acc b -> acc `shiftL` 1 .|. fromEnum b) 0

---

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

fst'' :: (a, b, c, d) -> a
fst'' (val1, val2, val3, val4) = val1

snd'' :: (a, b, c, d) -> b
snd'' (val1, val2, val3, val4) = val2

trd'' :: (a, b, c, d) -> c
trd'' (val1, val2, val3, val4) = val3

-- Obtiene el cuarto elemento de una tupla de cuatro elementos
frt'' :: (a, b, c, d) -> d 
frt'' (val1, val2, val3, val4) = val4

-- Obtiene el primer elemento de un "objeto" de tipo Tripleta
primerElemento :: Tripleta a -> a
primerElemento (Tripleta (p, _, _)) = p

-- Obtiene el segundo elemento de un "objeto" de tipo Tripleta
segundoElemento :: Tripleta a -> a
segundoElemento (Tripleta (_, s, _)) = s

-- Obtiene el tercer elemento de un "objeto" de tipo Tripleta
tercerElemento :: Tripleta a -> a
tercerElemento (Tripleta (_, _, t)) = t

-- Intercambia los elementos de una tupla:
intercambia :: (a, b) -> (b, a)
intercambia (x,y) = (y,x)

-- Obtiene el primer elemento de una lista
primero :: [a] -> a
primero = L.head

-- Obtiene el último elemento de una lista
ultimo :: [a] -> a
ultimo = L.last

obtieneElemento :: [a] -> Int -> a
obtieneElemento lista i = lista !! i

obtieneSubLista :: [[a]] -> Int -> [a]
obtieneSubLista listas i = listas !! i

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

numeroDigitos :: Int -> Int
numeroDigitos n = L.length $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = L.genericLength $ digitos n

elementoCentral :: [Int] -> Int
elementoCentral lista = lista !! pos
  where
    tamLista = L.length lista
    pos = abs $ div tamLista 2


eliminaCaracterEspecial :: Mensaje -> Mensaje
eliminaCaracterEspecial = L.filter (/='\n')

-- Obtiene el número de bits de un entero
numeroBits :: Integer -> Int
numeroBits numero
    | numero>0 = succ $ integerLog2 numero
    | otherwise = error "El número es menor que 0."

-- Comprueba si un caracter está dentro de caracteres
estaEnCaracteres :: Char -> Bool
estaEnCaracteres caracter = caracter `L.elem` caracteres

estaEnAsociaciones :: Int -> Bool
estaEnAsociaciones numero = numero `L.elem` numerosAsociados
    where
        numerosAsociados = L.map snd asociaciones

estaEnAbecedario :: String -> Bool
estaEnAbecedario xs = not (L.null xs) && L.all (`L.elem` abecedario) xs

-- Obtiene el índice de un elemento de caracteres
obtieneIndice :: Char -> Int
obtieneIndice caracter = primero [i | (c,i)<-asociaciones, c==caracter]