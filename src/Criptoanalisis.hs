module Criptoanalisis where

import Test.QuickCheck
import System.Random
import Util
import Tipos

--Comprueba que las claves son números primos
compruebaPrimalidadClaves :: Integer -> Integer -> Bool
compruebaPrimalidadClaves c1 c2 = undefined

-- Test de Miller Rabin para saber si un número es primo:
--      0: es compuesto
--      1: es probable primo
-- [Pág. 45]
{- Pseudocódigo:
    Se elige al azar un entero a con 1<a<n
    y = a^r mod n
    Si y=1 mod n, retorna 1
    j=1
    Mientras j<=(s-1) y y/=1:
        y=y^2 mod n
        Si y=n-1 retorna 1
        j=j+1
    Retorna 0 -}
testMillerRabin :: Int -> Integer -> String
testMillerRabin semilla n = millerRabin' semilla n 1

millerRabin' :: Int -> Integer -> Int -> String
millerRabin' semilla n j 
    | y==mod 1 (fromInteger n) = "Es probable primo."
    | otherwise = undefined
    where 
        y = generaAleatorio semilla 1 (fromInteger n)


millerRabin :: Integer -> Integer
millerRabin = undefined


{- [Pág. 46] Elige dos primos r,s de tamaño adecuado.
Calcula t como el menor primo de la forma a*r+1 con a entero.
Calcula p_0=(s^(t-1)-t^(s-1))mod(s*t).
Calcula p como el menor primo de la forma p=p_0+a*t*s con a entero. -}

testGordon = undefined

-- aleatoriedad en haskell
-- testPrimalidadFermat :: Integer -> Integer -> String
-- testPrimalidadFermat n k = if b^n /= modulo then "Compuesto" else "Probable primo"
--     where lista = [x | x<-[2..(n-1)]]
--           b = generaAleatorioLista lista
--           modulo = mod b n