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

primos :: (Integral a, Num a) => [a]
primos = criba [2..]

criba :: (Integral a, Num a) => [a] -> [a]
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

-- Algoritmo de euclídes
euclides :: (Integral a, Num a) => a -> a -> a
euclides a b
    | b == 0 = a
    | otherwise = euclides b (mod a b)

--Algoritmo extendido de Euclides
-- euclidesExtendido :: (Integral a, Num a) => a -> a -> Tripleta a
-- euclidesExtendido a b
--   | b == 0 = Tripleta (1, 0, a)
--   | otherwise = Tripleta (d, x, y - div a b * x)
--   where
--     Tripleta (d, x, y) = euclidesExtendido b (mod a b)

euclidesExtendido :: Integral a => a -> a -> Tripleta a
euclidesExtendido a 0 = Tripleta (1, 0, a)
euclidesExtendido a b = Tripleta (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        Tripleta (s, t, g) = euclidesExtendido b r

--Inverso de un número módulo n
-- inverso :: (Integral a, Num a) => a -> a -> a
-- inverso a n
--     | d /= 1  = error "El numero no tiene inverso modulo n."
--     | otherwise = mod x n
--     where
--         tripleta = euclidesExtendido a n
--         (d, x) = (primerElemento tripleta, segundoElemento tripleta)

inversoModular :: Integer -> Integer -> Integer
inversoModular a m
    | x < 0 = x + m
    | otherwise = x
    where
        Tripleta (x, _, _) = euclidesExtendido a m

inversoModular' :: Integer -> Integer -> Integer
inversoModular' a m
    | x < 0 = x `mod` m
    | otherwise = x
    where
        Tripleta (x, _, _) = euclidesExtendido a m

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

transformaTextoEnBinario :: Mensaje -> [Int]
transformaTextoEnBinario = L.concatMap transformaCaracterEnBinario

transformaCaracterEnBinario :: Char -> [Int]
transformaCaracterEnBinario c
    | estaEnCaracteres c = if L.length numeroAsociadoBinario<8
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
        numeroCeros = bits - L.length ns

transformaBinarioEnCaracter :: [Int] -> Char
transformaBinarioEnCaracter bs
    | numero>=10 && numero<ultimoCaracterAsociaciones = L.head [car | (car,num)<-asociaciones, num==numero]
    | otherwise = error "El numero introducido no esta dentro de la lista de asociaciones."
    where
        numero = deListaBinarioANum bs

transformaBinarioEnTexto :: [[Int]] -> Mensaje
transformaBinarioEnTexto = L.map transformaBinarioEnCaracter

transformaCaracterEnInt :: Char -> [Int]
transformaCaracterEnInt c
    | estaEnCaracteres c = [numeroAsociado]
    | otherwise = error "El caracter no es valido."
    where
        numeroAsociado = L.head [num | (car,num)<-asociaciones, car==c]

transformaTextoEnEntero :: Mensaje -> [Int]
transformaTextoEnEntero = L.concatMap transformaCaracterEnInt

transformaIntEnCaracter :: Int -> Char
transformaIntEnCaracter n
    | L.null listaCaracteres = error "El número no está asociado a algún carácter."
    | otherwise = primero listaCaracteres
    where
        listaCaracteres = [c | (c,num)<-asociaciones, num==n]

transformaEnteroEnTexto :: [Int] -> Mensaje
transformaEnteroEnTexto = L.map transformaIntEnCaracter

transformaListaNumerosEnNumero :: [Int] -> Int
transformaListaNumerosEnNumero ns = read $ L.concat [show $ obtieneElemento ns i | (n,i)<-L.zip ns [0..L.length ns-1]]

transformaListaNumeros :: [[Int]] -> [Int]
transformaListaNumeros = L.map transformaListaNumerosEnNumero

-- Transforma un número entero a una lista de bits
deIntA8Bits :: Int -> [Int]
deIntA8Bits n = L.reverse $ L.take 8 [convierteACerosYUnos (testBit n i) | i <- [0..finiteBitSize n - 1]]

deIntA64Bits :: Int -> [Int]
deIntA64Bits n = L.reverse $ L.take 64 [convierteACerosYUnos (testBit n i) | i <- [0..finiteBitSize n - 1]]

convierteACerosYUnos :: Bool -> Int
convierteACerosYUnos b = if b then 1 else 0

de8BitsA1Byte :: [[Int]] -> [[Int]]
de8BitsA1Byte [] = []
de8BitsA1Byte listaBits = L.concat (L.take 8 listaBits) : de8BitsA1Byte (L.drop 8 listaBits)

compruebaUltimoBloque :: [Int] -> Bool
compruebaUltimoBloque lista = L.length lista < 8

agregaCerosAlFinal :: [Int] -> [Int]
agregaCerosAlFinal lista
    | numeroCeros>0 = agregaCerosAlFinal (lista L.++ [0])
    | otherwise = lista
    where
        numeroCeros = 64 - L.length lista

--- Preparativos mensajes de Criptografía desde el punto de vista de la programación funcional:

-- Primera versión solo para letras mayúsculas sin espacios ni otros caracteres en el mensaje:

-- Cada número n entre 0 y 25 devuelve su correspondiente letra mayúscula
int2char :: Int -> Char
int2char n = chr (n + 65)

-- A cada letra le corresponde su posición en el abecedario
char2int :: Char -> Int
char2int c = ord c - 65

-- A cada lista de posiciones le asocia el mensaje correspondiente
int2str :: [Int] -> String
int2str = L.map int2char

-- La función inversa
str2int :: String -> [Int]
str2int = L.map char2int

-- Segunda versión: asociaciones de cada letra con un número binario:

-- Dado un entero, devuelve su código en binario
int2bin :: Int -> [Int]
int2bin n
    | n < 2 = [n]
    | otherwise = n `mod` 2 : int2bin (n `div` 2)

bin2int :: [Int] -> Int
bin2int = L.foldr (\x y -> x + 2*y) 0

-- Codificación y decoficación:

creaOcteto :: [Int] -> [Int]
creaOcteto bs = L.take 8 (bs L.++ repeat 0)

codificaEnBinario :: Mensaje -> [Int]
codificaEnBinario = L.concatMap (creaOcteto . int2bin . ord)

separaOctetos :: [Int] -> [[Int]]
separaOctetos [] = []
separaOctetos bs = L.take 8 bs : separaOctetos (L.drop 8 bs)

descodifica :: [Int] -> Mensaje
descodifica = L.map (chr . bin2int) . separaOctetos

-- Tercera asociación para el algoritmo de cifrado de bloques basado en AC:
-- Es necesario codificar la información en bloques de 64 bits, por lo que se usarán los siguientes algoritmos para lograrlo:

int2bin64' :: Integral a => a -> [a]
int2bin64' n
    | n < 2 = [n]
    | otherwise = int2bin64' (n `div` 2) L.++ [n `mod` 2]

largo64 :: Num a => [a] -> [a]
largo64 xs
    | L.length xs >= 64 = xs
    | otherwise = largo64 (0:xs)

int2bin64 :: Integral a => a -> [a]
int2bin64 = largo64 . int2bin64'

bin2int64 :: [Int] -> Int
bin2int64 xs = L.foldr (\x y -> x + 2*y) 0 (L.reverse xs)

strToInteger1 :: String -> Integer
strToInteger1 cs = L.foldl (\x y -> toInteger y + 1000 * toInteger x) 0 (L.map ord cs)

-- Más eficiente:
strToInteger :: String -> Integer
strToInteger = read . L.concatMap aux
    where
        aux c
            | c < '\n' = "00" L.++ cs
            | c < 'd' = '0' : cs
            | otherwise = cs
            where
                cs = show (ord c)

integerToStr1 :: Integer -> String
integerToStr1 0 = []
integerToStr1 n = integerToStr1 (div n 1000) L.++ [chr (fromIntegral (rem n 1000))]

-- Más eficiente:
integerToStr :: Integer -> String
integerToStr m
    | x == 0 = aux s
    | x == 1 = chr (read (L.take 1 s)) : aux (L.drop 1 s)
    | x == 2 = chr (read (L.take 2 s)) : aux (L.drop 2 s)
    where
        x = rem (L.length (show m)) 3
        s = show m
        aux n = L.map (chr . read) (parte 3 n)

--- FIN preparativos mensajes

    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

-- Introduce dos números en una tupla
introduceEnTupla :: (Integral a, Num a) => a -> a -> (a, a)
introduceEnTupla p q = (p,q)

xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

-- XOR aplicado a dos listas, número a número (bit a bit)
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
