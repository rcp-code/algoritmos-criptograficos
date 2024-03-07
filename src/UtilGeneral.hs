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
import Data.Bits
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
    | pos >= 0 && pos < tamLista lista = True
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
            | num < 2 = L.reverse (1:listaRestos)                               --se da la vuelta a la lista de restos y se añade un 1 al comienzo al llegar a 1
            | otherwise = cambioABase2' (div num 2) (mod num 2:listaRestos)     --se añade el resto a la lista de los restos ya calculados  

cambioABase2Lista :: (Integral a, Num a) => [a] -> [[a]]
cambioABase2Lista ns = [cambioABase2 x | x<-ns]

deListaBinarioANum :: Num a => [a] -> a
deListaBinarioANum ns = abs $ L.sum [(2^p)*x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

---

-- -- Convierte un número en binario a su representación de texto
-- binarioATexto :: Int -> Mensaje
-- binarioATexto numero
--   | numero == 0 = "0"
--   | otherwise = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (intToChar (x .&. 0xFF), x `shiftR` 8)) numero

-- binarioATexto' :: Int -> String
-- binarioATexto' numBinario
--   | numBinario == 0 = "0"
--   | otherwise = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (chr (fromIntegral (x .&. 0xFF)), x `shiftR` 8)) numBinario

-- -- Convierte un entero a un carácter
-- intToChar :: Int -> Char
-- intToChar = chr . fromIntegral


-- Tamaño de bits de un bloque de texto:
compruebaTamBits :: Mensaje -> Int
compruebaTamBits msg = finiteBitSize (traduceTextoABinario msg)

traduceTextoABinario :: Mensaje -> Int
traduceTextoABinario = P.foldl (\acc c -> (acc `shiftL` 8) .|. ord c) 0

traduceTextoABinario' :: Mensaje -> [Bool]
traduceTextoABinario' = L.concatMap caracteresABits
  where
    caracteresABits c = L.reverse [testBit (ord c) i | i <- [0..7]]

-- Convierte una lista de bits a su representación de texto
binarioATexto :: [Bool] -> Mensaje
binarioATexto = L.unfoldr (\x -> if L.null x then Nothing else Just (chr (bitsAByte (L.take 8 x)), L.drop 8 x))

-- Convierte una lista de bits a un byte
bitsAByte :: [Bool] -> Int
bitsAByte = L.foldl (\acc b -> acc * 2 + fromEnum b) 0

-- Convierte una lista de bits a un número entero
deBitsAInt :: [Bool] -> Int
deBitsAInt = L.foldl (\acc b -> acc `shiftL` 1 .|. fromEnum b) 0

-- Transforma un número entero a una lista de bits
deIntABits :: Int -> [Bool]
deIntABits n = L.reverse [testBit n i | i <- [0..finiteBitSize n - 1]]

---

deStringAInt :: String -> Int
deStringAInt [] = 0
deStringAInt c@(s:ss)
    | s == '-' = (-1) * deStringAInt ss
    | otherwise = (digitToInt s * 10 ^ (L.length c - 1)) + deStringAInt ss

    {----------------------------------------------------------------------
                        Tratamiento de datos
    ----------------------------------------------------------------------}

--Obtiene el primer elemento de una tupla de tres elementos
fst' :: (Num a, Num b, Num c) => (a, b, c) -> a
fst' (val1, val2, val3) = val1

--Obtiene el segundo elemento de una tupla de tres elementos
snd' :: (Num a, Num b, Num c) => (a, b, c) -> b
snd' (val1, val2, val3) = val2

--Obtiene el tercer elemento de una tupla de tres elementos
trd' :: (Num a, Num b, Num c) => (a, b, c) -> c
trd' (val1, val2, val3) = val3

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
numeroDigitos n = tamLista $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = tamListaGen $ digitos n

elementoCentral :: [Int] -> Int
elementoCentral lista = lista !! pos
  where
    pos = abs $ div (tamLista lista) 2

--Obtiene el tamaño de una lista
tamLista :: [a] -> Int
tamLista = L.length

tamListaGen :: [a] -> Integer
tamListaGen = L.genericLength

eliminaCaracterEspecial :: Mensaje -> Mensaje
eliminaCaracterEspecial = L.filter (/='\n')

-- Obtiene el número de bits de un entero
numeroBits :: Integer -> Int
numeroBits numero
    | numero>0 = succ $ integerLog2 numero
    | otherwise = error "El número es menor que 0."

-- Comprueba si un caracter está dentro de caracteres
estaEnCaracteres :: Char -> String -> Bool
estaEnCaracteres caracter cs = caracter `L.elem` cs

-- Obtiene el índice de un elemento de caracteres
obtieneIndice :: Char -> Int
obtieneIndice caracter = primero [i | (c,i)<-asociaciones, c==caracter]