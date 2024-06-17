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
import Data.Bits


    {----------------------------------------------------------------------
                    Aritmética modular y números primos
    ----------------------------------------------------------------------}

--Se obtiene el grupo multiplicativo del número p
obtieneGrupoMultiplicativo :: (Integral a, Num a) => a -> [a]
obtieneGrupoMultiplicativo p = P.filter (\x -> gcd x p == 1) [1..p-1]

--Obtiene los factores primos de un número p
factoresPrimos :: (Integral a, Num a) => a -> [a]
factoresPrimos p = filtraPrimos [2..p]
    where
        filtraPrimos [] = []
        filtraPrimos (p:ps) = p : filtraPrimos [x | x <- ps, x `mod` p /= 0]

--Comprueba si un número es probable primo
esPrimo :: (Integral a, Num a) => a -> Bool
esPrimo n
    | n <= 1 = False
    | n == 2 || n == 3 = True
    | even n || mod n 3==0 = False
    | otherwise = not $ L.any (\x -> n `mod` x == 0) [5, 11..raiz n]
    where
        raiz = floor . sqrt . fromIntegral

--Comprueba si dos números son coprimos
sonCoprimos :: (Integral a, Num a) => a -> a -> Bool
sonCoprimos x n = gcd x n == 1

--Calcula un primo seguro
primoSeguro :: (Integral a, Num a) => a -> a
primoSeguro p = 2*p+1

--Comprueba si un número es compuesto
esCompuesto :: (Integral a, Num a) => a -> Bool
esCompuesto n = L.any (\x -> mod n x == 0) [2..mitad]
    where
        mitad = div n 2

--Obtiene el primo más cercano (por debajo) si el número introducido no es primo
obtienePrimoCercanoInf :: (Integral a, Num a) => a -> a
obtienePrimoCercanoInf num
  | esPrimo num = num
  | otherwise = obtienePrimoCercanoInf (num-1)

--Obtiene el primo más cercano (por encima) si el número introducido no es primo
obtienePrimoCercanoSup :: (Integral a, Num a) => a -> a
obtienePrimoCercanoSup num
  | esPrimo num = num
  | otherwise = obtienePrimoCercanoSup (num+1)

--Obtiene todos los primos a partir de la criba de Eratóstenes
primos :: (Integral a, Num a) => [a]
primos = criba [2..]

--Criba de Eratóstenes
criba :: (Integral a, Num a) => [a] -> [a]
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

-- Algoritmo de Euclídes
euclides :: (Integral a, Num a) => a -> a -> a
euclides a b
    | b == 0 = a
    | otherwise = euclides b (mod a b)

--Algoritmo de Euclídes extendido
euclidesExtendido :: Integral a => a -> a -> (a, a, a)
euclidesExtendido a 0 = (1, 0, a)
euclidesExtendido a b = (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        (s, t, g) = euclidesExtendido b r

--Cálculo del inverso modular
inversoModular' :: Integer -> Integer -> Integer
inversoModular' a m
    | x < 0 = x `mod` m
    | otherwise = x
    where
        (x, _, _) = euclidesExtendido a m

    {----------------------------------------------------------------------
                    Funciones auxiliares para los mensajes
    ----------------------------------------------------------------------}

-- Parte una lista en n listas
parte :: Int -> [a] -> [[a]]
parte _ [] = []
parte n xs = L.take n xs:parte n (L.drop n xs)

--Transforma texto en binario
transformaTextoEnBinario :: Mensaje -> [Int]
transformaTextoEnBinario = L.concatMap transformaCaracterEnBinario

--Transforma un carácter a binario
transformaCaracterEnBinario :: Char -> [Int]
transformaCaracterEnBinario c
    | estaEnCaracteres c = if L.length numeroAsociadoBinario<8
                                        then agregaCerosAIzquierda numeroAsociadoBinario 8
                                      else numeroAsociadoBinario
    | otherwise = error "El caracter no es valido."
    where
        numeroAsociado = L.head [num | (car,num)<-asociaciones, car==c]
        numeroAsociadoBinario = cambioABase2 numeroAsociado

--Agrega ceros a la izquierda a un número binario de 8 bits
agregaCerosAIzquierda :: [Int] -> Int -> [Int]
agregaCerosAIzquierda ns bits
    | numeroCeros>0 = agregaCerosAIzquierda (0:ns) bits
    | otherwise = ns
    where
        numeroCeros = bits - L.length ns

--Transforma un número binario en su equivalente carácter
transformaBinarioEnCaracter :: [Int] -> Char
transformaBinarioEnCaracter bs
    | numero>=10 && numero<ultimoCaracterAsociaciones = L.head [car | (car,num)<-asociaciones, num==numero]
    | otherwise = error "El numero introducido no esta dentro de la lista de asociaciones."
    where
        numero = deListaBinarioANum bs

--Transforma un número binario en el texto correspondiente
transformaBinarioEnTexto :: [[Int]] -> Mensaje
transformaBinarioEnTexto = L.map transformaBinarioEnCaracter

--Transforma un carácter en su equivalente numérico
transformaCaracterEnInt :: Char -> [Int]
transformaCaracterEnInt c
    | estaEnCaracteres c = [numeroAsociado]
    | otherwise = error "El caracter no es valido."
    where
        numeroAsociado = L.head [num | (car,num)<-asociaciones, car==c]

--Transforma un texto en entero
transformaTextoEnEntero :: Mensaje -> [Int]
transformaTextoEnEntero = L.concatMap transformaCaracterEnInt

--Transforma un entero en su equivalente carácter
transformaIntEnCaracter :: Int -> Char
transformaIntEnCaracter n
    | L.null listaCaracteres = error "El numero no esta asociado a algun caracter."
    | otherwise = primero listaCaracteres
    where
        listaCaracteres = [c | (c,num)<-asociaciones, num==n]

--Transforma un entero en texto
transformaEnteroEnTexto :: [Int] -> Mensaje
transformaEnteroEnTexto = L.map transformaIntEnCaracter

--Transforma una lista de enteros en texto
transformaListaEnTexto :: [Int] -> Mensaje
transformaListaEnTexto ns = L.map transformaIntEnCaracter lss
    where 
        lss = L.concat $ parte 2 ns

--Transforma una lista de números en un número
transformaListaNumerosEnNumero :: [Int] -> Int
transformaListaNumerosEnNumero ns = read $ L.concat [show $ obtieneElemento ns i | (n,i)<-L.zip ns [0..L.length ns-1]]

--Transforma una lista de números en un número
transformaListaNumeros :: [[Int]] -> [Int]
transformaListaNumeros = L.map transformaListaNumerosEnNumero

--Transforma un número entero a una lista de 8 bits
deIntA8Bits :: Int -> [Int]
deIntA8Bits n = L.reverse $ L.take 8 [convierteACerosYUnos (testBit n i) | i <- [0..finiteBitSize n - 1]]

--Transforma un número entero a una lista de 64 bits
deIntA64Bits :: Int -> [Int]
deIntA64Bits n = L.reverse $ L.take 64 [convierteACerosYUnos (testBit n i) | i <- [0..finiteBitSize n - 1]]

--Convierte un booleano (True o False) en 1 o 0
convierteACerosYUnos :: Bool -> Int
convierteACerosYUnos b = if b then 1 else 0

--Convierte 8 bits en 1 Byte
de8BitsA1Byte :: [[Int]] -> [[Int]]
de8BitsA1Byte [] = []
de8BitsA1Byte listaBits = L.concat (L.take 8 listaBits) : de8BitsA1Byte (L.drop 8 listaBits)

--Comprueba el tamaño del último bloque
compruebaTamUltimoBloque :: [Int] -> Bool
compruebaTamUltimoBloque lista = L.length lista < 8

--Agrega 0 al final de una lista
agregaCerosAlFinal :: [Int] -> [Int]
agregaCerosAlFinal lista
    | numeroCeros>0 = agregaCerosAlFinal (lista L.++ [0])
    | otherwise = lista
    where
        numeroCeros = 64 - L.length lista

--Preparativos mensajes: asociaciones de cada letra con un número binario

--Dado un entero, devuelve su código en binario
int2bin :: Int -> [Int]
int2bin n
    | n < 2 = [n]
    | otherwise = n `mod` 2 : int2bin (n `div` 2)

--Dado un número binario, devuelve su equivalente entero
bin2int :: [Int] -> Int
bin2int = L.foldr (\x y -> x + 2*y) 0

-- Codificación y descodificación:
creaOcteto :: [Int] -> [Int]
creaOcteto bs = L.take 8 (bs L.++ repeat 0)

codificaEnBinario :: Mensaje -> [Int]
codificaEnBinario = L.concatMap (creaOcteto . int2bin . ord)

separaOctetos :: [Int] -> [[Int]]
separaOctetos [] = []
separaOctetos bs = L.take 8 bs : separaOctetos (L.drop 8 bs)

descodifica :: [Int] -> Mensaje
descodifica = L.map (chr . bin2int) . separaOctetos

    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

--Introduce dos números en una tupla
introduceEnTupla :: (Integral a, Num a) => a -> a -> (a, a)
introduceEnTupla p q = (p,q)

--Construye una clave introduciendo dos números en una tupla de tipo Clave
construyeClave :: Integer -> Integer -> Clave
construyeClave p q = (p,q)

--Realiza la operación XOR entre dos elementos
xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

-- XOR bit a bit aplicado a listas
xorl :: [Int] -> [Int] -> [Int]
xorl [] _ = []
xorl _ [] = []
xorl datos1 datos2 = [xor2 x y | (x,y)<-L.zip datos1 datos2]
    where
        xor2 a b
            | xor' x y = 1
            | otherwise = 0
            where
                x = a==1
                y = b==1

    {----------------------------------------------------------------------
                            Funciones de aleatoriedad
    ----------------------------------------------------------------------}

--Genera una lista de números aleatorios
generaAleatoriosL :: Int -> [Int]
generaAleatoriosL semilla = L.concat [generaAleatorios semilla | _ <-[1..100]]
    where
        lista = L.take 1000000000000 $ randoms (mkStdGen semilla)
        generaAleatorios :: Int -> [Int]
        generaAleatorios semilla = [mod x 1000000000000 | x<-lista]

--Genera un número aleatorio entre dos valores
generaAleatorio :: Int -> Int -> Int -> Int
generaAleatorio semilla vMin vMax = fst $ randomR (vMin, vMax) (mkStdGen semilla)

--Genera un número aleatorio entre 0 y 1
generaAleatorio' :: Int -> Int
generaAleatorio' semilla = fst $ randomR (0, 1) (mkStdGen semilla)
