module Util where

    {----------------------------------------------------------------------
            Incluye algunas funciones auxiliares o de utilidad
            para distintos ficheros, de forma que queden todos
                    unificados en un mismo lugar.
    ----------------------------------------------------------------------}

import Data.Vector as V
import Prelude as P
import Data.Char
import Data.List as L
import System.Random
import Data.Bits
import Data.Word

import Tipos
import Math.NumberTheory.Logarithms

    {----------------------------------------------------------------------
                        Definición de "constantes"
    ----------------------------------------------------------------------}

caracteres :: String
caracteres = ' ' : ['a'..'z'] P.++ ['A'..'Z'] P.++ ['0'..'9'] P.++ ['.',',']

numeros :: [Int]
numeros = [0..n]
    where n = L.genericLength caracteres-1

asociaciones :: [(Char, Int)]
asociaciones = L.zip caracteres numeros

    {----------------------------------------------------------------------
                        Transformaciones de datos
    ----------------------------------------------------------------------}

--Transforma un número en una lista de sus dígitos
digitos :: Int -> [Int]
digitos n = [read [x] :: Int | x<-show n]

--Transforma una lista de dígitos en el número que corresponde
deDigitosAInt :: [Int] -> Int
deDigitosAInt ns = deDigitosAInt' (L.reverse ns)
    where
        deDigitosAInt' [x] = x
        deDigitosAInt' (x:xs) = x+10*deDigitosAInt' xs

deDigitosAEntero :: [Integer] -> Integer
deDigitosAEntero ns = deDigitosAEntero (L.reverse ns)
    where
        deDigitosAEntero' [x] = x
        deDigitosAEntero' (x:xs) = x+10*deDigitosAEntero' xs

cambioABase2 :: (Integral a, Num a) => a -> [a]
cambioABase2 num = cambioABase2' num []
    where
        cambioABase2' num listaRestos
            | num < 2 = listaAux
            | otherwise = cambioABase2' cociente restos
            where
                cociente = div num 2
                resto = mod num 2
                restos = resto:listaRestos              --se añade el resto a la lista de los restos ya calculados
                listaAux = L.reverse (1:listaRestos)    --se da la vuelta a la lista de restos y se añade un 1 al comienzo al llegar a 1

cambioABase2Lista :: (Integral a, Num a) => [a] -> [[a]]
cambioABase2Lista ns = [cambioABase2 x | x<-ns]

cambiaListaIntABase2 :: [Int] -> [[Int]]
cambiaListaIntABase2 ns = [cambiaIntABase2 x | x<-ns]
    where
        cambiaIntABase2 num = cambiaIntABase2' num []
        cambiaIntABase2' num lrs
            | num < 2 = listaAux
            | otherwise = cambiaIntABase2' cociente restos
            where
                cociente = num `div` 2
                resto = num `mod` 2
                restos = resto:lrs
                listaAux = L.reverse (1:lrs)

deListaBinarioAInt :: [Int] -> Int
deListaBinarioAInt ns = abs $ L.sum [(2^p)*fromIntegral x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

deListaBinarioANum :: Num a => [a] -> a
deListaBinarioANum ns = abs $ L.sum [(2^p)*x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

--Transforma una lista (1 y 0) en un entero (generalizado)
transformaBits :: (Integral a, Num a) => [a] -> a
transformaBits = P.foldl ((+) . (2 *)) 0

--Transforma una lista (1 y 0) en un entero
transformaBits' :: [Int] -> Int
transformaBits' = P.foldl ((+) . (2 *)) 0

deListaAVector :: [a] -> Vector a
deListaAVector = V.fromList

deVectorALista :: Vector a -> [a]
deVectorALista = V.toList

deCharAInt :: Char -> Int
deCharAInt c
    | isLower c = ord c - ord 'a'
    | isUpper c = ord c - ord 'A'
    | isDigit c = ord c - ord '0'
    | otherwise = ord ' '

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
primerElemento (Tripleta 3 (p, _, _)) = p

-- Obtiene el segundo elemento de un "objeto" de tipo Tripleta
segundoElemento :: Tripleta a -> a
segundoElemento (Tripleta 3 (_, s, _)) = s

-- Obtiene el tercer elemento de un "objeto" de tipo Tripleta
tercerElemento :: Tripleta a -> a
tercerElemento (Tripleta 3 (_, _, t)) = t

numeroDigitos :: Int -> Int
numeroDigitos n = L.length $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = L.genericLength $ digitos n

obtieneIndiceLista :: (Eq a) => [a] -> a -> Int
obtieneIndiceLista ls elemento
    | elemento `L.notElem` ls = error "No se ha podido obtener el indice porque el elemento no se encuentra en la lista."
    | otherwise = indiceLista 0 ls elemento
    where
        indiceLista ind [] e = -1
        indiceLista ind (l:ls) e
            | e==l = ind
            | ind<L.genericLength ls = indiceLista (ind+1) (L.drop 1 ls) e
            | otherwise = indiceLista ind ls e

--Obtiene el tamaño de una lista
tamLista :: Num a => [a] -> Int
tamLista = L.length

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
obtieneIndice caracter = L.head [i | (c,i)<-asociaciones, c==caracter]

    {----------------------------------------------------------------------
                    Aritmética modular y números primos
    ----------------------------------------------------------------------}

--Se obtiene el grupo multiplicativo del número p
obtieneGrupoMultiplicativo :: Integer -> [Integer]
obtieneGrupoMultiplicativo p = P.filter (\x -> gcd x p == 1) [1..p-1]

factores :: Integer -> [Integer]
factores n
    | n<=1 = []
    | otherwise = factoriza n 2
    where
        factoriza :: Integer -> Integer -> [Integer]
        factoriza 1 _ = []
        factoriza m c
            | mod m c==0 = c:factoriza (div m c) c
            | otherwise = factoriza m (c+1)

-- Obtiene los factores primos de un número p
factoresPrimos :: Integer -> [Integer]
factoresPrimos p = filtraPrimos [2..p]
    where
        filtraPrimos [] = []
        filtraPrimos (p:ps) = p : filtraPrimos [x | x <- ps, x `mod` p /= 0]

-- Comprueba si un número es primo
esPrimo :: Integer -> Bool
esPrimo n
    | n<=1 = False
    | n==2 || n==3 = True
    | even n || mod n 3==0 = False
    | otherwise = not $ L.any (\x -> mod n x == 0) [5, 11..raiz n]
    where
        raiz = floor . sqrt . fromIntegral

--Función generalizada que comprueba si un número es primo
esPrimo' :: (Integral a, Num a) => a -> Bool
esPrimo' n
    | n <= 1 = False
    | n == 2 || n == 3 = True
    | even n || mod n 3==0 = False
    | otherwise = not $ L.any (\x -> n `mod` x == 0) [5, 11..raiz n]
    where
        raiz = floor . sqrt . fromIntegral

-- Comprueba si dos números son coprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x n = gcd x n == 1

-- Calcula un primo seguro
primoSeguro :: Integer -> Integer
primoSeguro p = 2*p+1

-- Comprueba si un número es compuesto
esCompuesto :: Integer -> Bool
esCompuesto n = L.any (\x -> mod n x == 0) [2..mitad]
    where
        mitad = div n 2

-- Algoritmo de euclides
euclides :: Integer -> Integer -> Integer
euclides a b
    | b == 0 = a
    | otherwise = euclides b (mod a b)

--Algoritmo extendido de Euclides
euclidesExtendido :: Integer -> Integer -> Tripleta Integer
euclidesExtendido a b
  | b == 0 = Tripleta 3 (a, 1, 0)
  | otherwise = Tripleta 3 (d, x, y - div a b * x)
  where
    Tripleta 3 (d, x, y) = euclidesExtendido b (mod a b)

--Inverso de un número módulo n
-- inverso :: Integer -> Integer -> Integer
-- inverso a n
--     | d /= 1  = error "El número no tiene inverso módulo n."
--     | otherwise = mod x n
--     where
--         (d, x) = (fst' (euclidesExtendido a n), snd' (euclidesExtendido a n))

--Inverso de un número módulo n
inverso :: Integer -> Integer -> Integer
inverso a n
    | d /= 1  = error "El numero no tiene inverso modulo n."
    | otherwise = mod x n
    where
        tripleta = euclidesExtendido a n
        (d, x) = (primerElemento tripleta, segundoElemento tripleta)

-- Función de euler
funcionEuler :: Integer -> Integer
funcionEuler n = L.sum [1 | p<-[1..n], sonCoprimos n p]

-- Comprueba si se cumple el teorema de Euler
teoremaEuler :: Integer -> Integer -> Bool
teoremaEuler x p = x^(p-1) == mod 1 p

-- Factorización de Fermat
factorizacionFermat :: Integer -> Integer
factorizacionFermat n = factorizacionFermat' x y2
    where
        raiz = sqrt (fromIntegral n)
        x = ceiling raiz
        cuad = x^2
        y2 = cuad-n

-- Función recursiva auxiliar
factorizacionFermat' :: Integer -> Integer -> Integer
factorizacionFermat' num aux
    | aux/=fromIntegral aux = factorizacionFermat' x' y2
    | otherwise = x' - round raiz
    where
        x' = x'+1
        y2 = x'^2-num
        raiz = sqrt (fromIntegral y2)

    {----------------------------------------------------------------------
                    Funciones auxiliares para los mensajes
    ----------------------------------------------------------------------}

-- Elimina los espacios de una cadena
eliminaEspacios :: String -> String
eliminaEspacios xs = [x | x<-xs, x/=' ']

trasponer :: Int -> [a] -> [a]
trasponer n [] = []
trasponer 0 xs = xs
trasponer n xs = trasponer (n-1) ys
    where ys = P.tail xs P.++ [P.head xs]

-- Parte en n trozos
parte :: Int -> [a] -> [[a]]
parte _ [] = []
parte n xs = L.take n xs:parte n (L.drop n xs)

-- Prepara un mensaje y lo transforma en una lista de enteros
-- preparaMensaje :: Mensaje -> [Integer]
-- preparaMensaje m = read $ L.concatMap (show . ord) m

-- Prepara un mensaje y lo transforma en un entero
-- preparaMensaje' :: Mensaje -> Integer
-- preparaMensaje' m = read $ L.concatMap (show . ord) m

preparaMensaje :: Mensaje -> [Integer]
preparaMensaje [] = []
preparaMensaje ms = preparaMensaje' ms []
    where
        preparaMensaje' "" auxs = auxs
        preparaMensaje' (c:ms) auxs
            | c `L.notElem` caracteres = error "Caracter invalido en el mensaje."
            | otherwise = preparaMensaje' (L.drop 1 ms) (listaTransf P.++ auxs)
            where
                indice = obtieneIndice c
                elemento = asociaciones !! indice
                numero = snd elemento
                listaTransf = introduceEnLista numero

-- Función auxiliar creada para preparaMensaje (en principio)
introduceEnLista :: Int -> [Integer]
introduceEnLista n
    | numeroDigitos n<2 = 0:[toInteger n]
    | otherwise = [toInteger (deStringAInt s) | s<-ns]
    where
        numero = toInteger n
        ns = parte 1 (show numero)

transformaEnEntero :: [Integer] -> Integer
transformaEnEntero [] = error "No se puede convertir a entero porque la lista esta vacia."
transformaEnEntero ls = deDigitosAEntero ls

-- Obtiene el mensaje a partir de una representación como número entero
-- deRepresentacion :: Integer -> Mensaje
-- deRepresentacion num = L.map (chr . read) (parte 2 $ show num)

deRepresentacion :: Integer -> Mensaje
deRepresentacion num = undefined
    where
        particiones = parte 3 (show num)


    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

-- Introduce dos números primos en una tupla
introducePrimosEnTupla :: (Integral a, Num a) => a -> a -> Tupla a
introducePrimosEnTupla p q = Tupla 2 (p,q)

-- Introduce dos números en una tupla
introduceEnTupla :: Integer -> Integer -> Tupla Integer
introduceEnTupla p q = Tupla 2 (p,q)

--Introduce dos números en una tupla (generalizado)
introduceEnTupla' :: (Integral a, Num a) => a -> a -> Tupla a
introduceEnTupla' p q = Tupla 2 (p,q)

construyeClave :: Integer -> Integer -> Clave
construyeClave p q = (p,q)


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