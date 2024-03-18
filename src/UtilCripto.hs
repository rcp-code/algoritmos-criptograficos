module UtilCripto where

    {----------------------------------------------------------------------
                Incluye funciones útiles generales relacionados
                                con Criptografía.
    ----------------------------------------------------------------------}

import Tipos
import UtilGeneral

import Data.Vector as V
import Data.Matrix as M
import Prelude as P
import Data.List as L
import Data.Char
import Constantes
import Data.Maybe
import System.Random


    {----------------------------------------------------------------------
                    Aritmética modular y números primos
    ----------------------------------------------------------------------}

--Se obtiene el grupo multiplicativo del número p
obtieneGrupoMultiplicativo :: (Integral a, Num a) => a -> [a]
obtieneGrupoMultiplicativo p = P.filter (\x -> gcd x p == 1) [1..p-1]

-- factores :: (Integral a, Num a) => a -> [a]
-- factores n
--     | n<=1 = []
--     | otherwise = factoriza n 2
--     where
--         factoriza 1 _ = []
--         factoriza m c
--             | mod m c==0 = c:factoriza (div m c) c
--             | otherwise = factoriza m (c+1)

-- Otra forma de calcular factores (incluye el 1)
-- factores' :: (Integral a, Num a) => a -> [a]
-- factores' n = [x | x <- [1..n], mod n x == 0]

-- Obtiene los factores primos de un número p
factoresPrimos :: (Integral a, Num a) => a -> [a]
factoresPrimos p = filtraPrimos [2..p]
    where
        filtraPrimos [] = []
        filtraPrimos (p:ps) = p : filtraPrimos [x | x <- ps, x `mod` p /= 0]

-- Comprueba si un número es primo
esPrimo :: (Integral a, Num a) => a -> Bool
esPrimo n
    | n <= 1 = False
    | n == 2 || n == 3 = True
    | even n || mod n 3==0 = False
    | otherwise = not $ L.any (\x -> n `mod` x == 0) [5, 11..raiz n]
    where
        raiz = floor . sqrt . fromIntegral

-- Otra forma de comprobar si un número es primo
-- esPrimo' :: (Integral a, Num a) => a -> Bool
-- esPrimo' n = factores n == [n]

-- Comprueba si dos números son coprimos
sonCoprimos :: (Integral a, Num a) => a -> a -> Bool
sonCoprimos x n = gcd x n == 1

-- Calcula un primo seguro
primoSeguro :: (Integral a, Num a) => a -> a
primoSeguro p = 2*p+1

-- Comprueba si un número es compuesto
esCompuesto :: (Integral a, Num a) => a -> Bool
esCompuesto n = L.any (\x -> mod n x == 0) [2..mitad]
    where
        mitad = div n 2

-- Obtiene el primo más cercano (por debajo), en caso de que el número introducido no sea primo
obtienePrimoCercanoInf :: (Integral a, Num a) => a -> a
obtienePrimoCercanoInf num
  | esPrimo num = num
  | otherwise = obtienePrimoCercanoInf (num-1)

-- Obtiene el primo más cercano (por encima), en caso de que el número introducido no sea primo
obtienePrimoCercanoSup :: (Integral a, Num a) => a -> a
obtienePrimoCercanoSup num
  | esPrimo num = num
  | otherwise = obtienePrimoCercanoSup (num+1)

-- Algoritmo de euclides
euclides :: (Integral a, Num a) => a -> a -> a
euclides a b
    | b == 0 = a
    | otherwise = euclides b (mod a b)

--Algoritmo extendido de Euclides
euclidesExtendido :: (Integral a, Num a) => a -> a -> Tripleta a
euclidesExtendido a b
  | b == 0 = Tripleta (a, 1, 0)
  | otherwise = Tripleta (d, x, y - div a b * x)
  where
    Tripleta (d, x, y) = euclidesExtendido b (mod a b)

--Inverso de un número módulo n
inverso :: (Integral a, Num a) => a -> a -> a
inverso a n
    | d /= 1  = error "El numero no tiene inverso modulo n."
    | otherwise = mod x n
    where
        tripleta = euclidesExtendido a n
        (d, x) = (primerElemento tripleta, segundoElemento tripleta)

-- Función de euler
funcionEuler :: (Integral a, Num a) => a -> a
funcionEuler n = L.sum [1 | p<-[1..n], sonCoprimos n p]

-- Comprueba si se cumple el teorema de Euler
teoremaEuler :: (Integral a, Num a) => a -> a -> Bool
teoremaEuler x p = x^(p-1) == mod 1 p

-- Factorización de Fermat
factorizacionFermat :: (Integral a, Num a) => a -> a
factorizacionFermat n = factorizacionFermat' x y2
    where
        raiz = sqrt (fromIntegral n)
        x = ceiling raiz
        cuad = x^2
        y2 = cuad-n

-- Función recursiva auxiliar
factorizacionFermat' :: (Integral a, Num a) => a -> a -> a
factorizacionFermat' num aux
    | aux/=fromIntegral aux = factorizacionFermat' x' y2
    | otherwise = x' - round raiz
    where
        x' = x'+1
        y2 = x'^2-num
        raiz = sqrt (fromIntegral y2)

construyeClave :: Integer -> Integer -> Clave
construyeClave p q = (p,q)


    {----------------------------------------------------------------------
                    Funciones auxiliares para los mensajes
    ----------------------------------------------------------------------}

-- Parte en n trozos
parte :: Int -> [a] -> [[a]]
parte _ [] = []
parte n xs = L.take n xs:parte n (L.drop n xs)

deStringAInteger :: String -> Integer
deStringAInteger = read . L.concatMap aux
    where aux c | c < 'd' = '0' : show (ord c)
                | otherwise = show (ord c)

deIntegerAString :: Integer -> String
deIntegerAString m
    | x == 0 = aux s
    | x == 1 = chr (read (L.take 1 s)) : aux (L.drop 1 s)
    | otherwise = chr (read (L.take 2 s)) : aux (L.drop 2 s)
    where
        x = rem (L.length (show m)) 3
        s = show m
        aux n = L.map (chr . read) (parte 3 n)

---

estaEnRangoASCII :: Int -> Bool
estaEnRangoASCII n = n >= ord ' ' && n <= ord '~'

deStringAInteger' :: String -> Integer
deStringAInteger' = read . L.concatMap aux
  where
    aux c = show (ord c)

deIntegerAString' :: Integer -> String
deIntegerAString' m
  | x == 0 = aux s
  -- | otherwise = chr (read (L.take 3 s)) : aux (L.drop 3 s)
  | otherwise = if estaEnRangoASCII num1
                   then chr num1 : aux (L.drop 3 s)
                   else error "El código no está dentro de la tabla ASCII."
  where
    x = rem (L.length (show m)) 3
    s = show m
    num1 = deDigitosANum $ L.take 3 (digitos (fromInteger m))
    num2 = deDigitosANum $ L.take 2 (digitos (fromInteger m))
    aux n = L.map (chr . read) (parte 3 n)

-- Funciones de depuración
mostrarRepresentacion :: String -> String
mostrarRepresentacion str = show (L.map ord str)

mostrarRepresentacionNumerica :: String -> String
mostrarRepresentacionNumerica str = show (deStringAInteger str)

---
-- Convierte un carácter a su valor numérico según su posición en asociaciones
deCaracterANumero :: Char -> [Int]
deCaracterANumero c = maybeToList $ L.lookup c asociaciones

-- Convierte una lista de caracteres a una lista de números
textoANumeros :: String -> [Int]
textoANumeros = P.concatMap deCaracterANumero

-- Función para preparar un texto antes de encriptar
prepararTexto :: String -> [Int]
prepararTexto = textoANumeros

-- Convierte un número a su carácter según asociaciones
deNumeroACaracter :: Int -> [Char]
deNumeroACaracter n = maybeToList $ L.lookup n $ L.map intercambia asociaciones         -- intercambia las posiciones en las tuplas en asociaciones y después busca el número para sacar el caracter que le corresponde

-- Convierte una lista de números a una lista de caracteres
deNumerosATexto :: [Int] -> Mensaje
deNumerosATexto = L.concatMap deNumeroACaracter

-- Función para restaurar el texto después de descifrar
restaurarTexto :: [Int] -> Mensaje
restaurarTexto = deNumerosATexto

preparaMensaje :: Mensaje -> [Int]
preparaMensaje "" = []
preparaMensaje ms = preparaMensaje' ms []

preparaMensaje' :: Mensaje -> [Int] -> [Int]
preparaMensaje' "" auxs = auxs
preparaMensaje' (c:ms) auxs
    | c `L.notElem` caracteres = error "Caracter invalido en el mensaje."
    | otherwise = preparaMensaje' ms (auxs P.++ listaTransf)
    where
        indice = obtieneIndice c
        elemento = asociaciones !! indice
        numero = snd elemento
        listaTransf = introduceEnLista numero

-- Función auxiliar que transforma un número en una lista con números de dos dígitos
introduceEnLista :: Int -> [Int]
introduceEnLista n = [deStringAInt s | s<-ns]
    where
        numero = toInteger n
        ns = parte 2 (show numero)

transformaEnNumero :: [Int] -> Int
transformaEnNumero [] = error "No se puede convertir a entero porque la lista esta vacia."
transformaEnNumero ns = deDigitosANum $ L.concat [digitos x | x<-ns]

deIntAString :: Integer -> String
deIntAString n
    | x==0 = L.concat traduccionDos
    | x==1 = L.concat traduccionUno
    | otherwise = error "No se puede traducir el número en una cadena de texto."
    where
        cadena = show n
        x = rem (L.length cadena) 2
        particionesDos = L.map deStringAInt (parte 2 cadena)
        particionesUno = L.map deStringAInt (parte 1 cadena)
        traduccionDos = L.map deNumeroACaracter particionesDos
        traduccionUno = L.map deNumeroACaracter particionesUno

    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

-- Introduce dos números en una tupla
introduceEnTupla :: (Integral a, Num a) => a -> a -> (a, a)
introduceEnTupla p q = (p,q)

xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

    {----------------------------------------------------------------------
                            Funciones de aleatoriedad
    ----------------------------------------------------------------------}

-- Genera una lista de números aleatorios
generaAleatoriosL :: Int -> [Int]
generaAleatoriosL semilla = L.concat [generaAleatorios semilla | _ <-[1..100]]
    where
        lista = L.take 1000000000000 $ randoms (mkStdGen semilla)
        generaAleatorios :: Int -> [Int]
        generaAleatorios semilla = [mod x 1000000000000 | x<-lista]

-- Genera un número aleatorio entre dos valores
generaAleatorio :: Int -> Int -> Int -> Int
generaAleatorio semilla vMin vMax = fst $ randomR (vMin, vMax) (mkStdGen semilla)

-- Genera un número aleatorio entre 0 y 1
generaAleatorio' :: Int -> Int
generaAleatorio' semilla = fst $ randomR (0, 1) (mkStdGen semilla)
