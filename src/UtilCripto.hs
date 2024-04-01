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

bloques64Bits :: [Int] -> [[Int]]
bloques64Bits = parte 64

transformaTextoEnBinario :: String -> [Int]
transformaTextoEnBinario = L.concatMap transformaCaracterEnBinario

transformaCaracterEnBinario :: Char -> [Int]
transformaCaracterEnBinario c
    | estaEnCaracteres c caracteres = if tamLista numeroAsociadoBinario<8
                                        then agregaCerosAIzquierda numeroAsociadoBinario 8
                                      else numeroAsociadoBinario
    | otherwise = error "El caracter no es valido."
    where
        numeroAsociado = L.head [num | (car,num)<-asociaciones, car==c]
        numeroAsociadoBinario = cambioABase2 numeroAsociado

-- Agrega ceros a la izquierda a un número binario de 8 bits
agregaCerosAIzquierda :: [Int] -> Int -> [Int]
agregaCerosAIzquierda ns bits
    | numeroCeros>0 = agregaCerosAIzquierda (0:ns) bits
    | otherwise = ns
    where
        numeroCeros = bits - tamLista ns

transformaBinarioEnCaracter :: [Int] -> Char
transformaBinarioEnCaracter bs 
    | numero>=10 && numero<ultimoCaracterAsociaciones = L.head [car | (car,num)<-asociaciones, num==numero]
    | otherwise = error "El numero introducido no esta dentro de la lista de asociaciones."
    where
        numero = deListaBinarioANum bs

transformaBinarioEnTexto :: [[Int]] -> String
transformaBinarioEnTexto = L.map transformaBinarioEnCaracter

transformaCaracterEnInt :: Char -> [Int]
transformaCaracterEnInt c
    | estaEnCaracteres c caracteres = [numeroAsociado]
    | otherwise = error "El caracter no es valido."
    where
        numeroAsociado = L.head [num | (car,num)<-asociaciones, car==c]

transformaTextoEnEntero :: String -> [Int]
transformaTextoEnEntero = L.concatMap transformaCaracterEnInt

transformaIntEnCaracter :: Int -> Char
transformaIntEnCaracter n = L.head [c | (c,num)<-asociaciones, num==n]

transformaEnteroEnTexto :: [Int] -> String
transformaEnteroEnTexto = L.map transformaIntEnCaracter

transformaListaNumerosEnNumero :: [Int] -> Int
transformaListaNumerosEnNumero ns = read $ L.concat [show $ obtieneElemento ns i | (n,i)<-L.zip ns [0..L.length ns-1]]

transformaListaNumeros :: [[Int]] -> [Int]
transformaListaNumeros = L.map transformaListaNumerosEnNumero

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
