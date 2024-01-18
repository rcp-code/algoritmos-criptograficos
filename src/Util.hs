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
deDigitosANum :: (Integral a, Num a) => [a] -> a
deDigitosANum ns = deDigitosANum' (L.reverse ns)
    where
        deDigitosANum' [x] = x
        deDigitosANum' (x:xs) = x+10*deDigitosANum' xs

cambioABase2 :: (Integral a, Num a) => a -> [a]
cambioABase2 num = cambioABase2' num []
    where
        cambioABase2' num listaRestos
            | num < 2 = L.reverse (1:listaRestos)                               --se da la vuelta a la lista de restos y se añade un 1 al comienzo al llegar a 1
            | otherwise = cambioABase2' (div num 2) (mod num 2:listaRestos)   --se añade el resto a la lista de los restos ya calculados  

cambioABase2Lista :: (Integral a, Num a) => [a] -> [[a]]
cambioABase2Lista ns = [cambioABase2 x | x<-ns]

deListaBinarioANum :: Num a => [a] -> a
deListaBinarioANum ns = abs $ L.sum [(2^p)*x | (x,p)<-union]
    where
        tam = L.length ns
        ps = [0..tam-1]
        union = L.zip ns (L.reverse ps)

deListaAVector :: [a] -> Vector a
deListaAVector = V.fromList

deVectorALista :: Vector a -> [a]
deVectorALista = V.toList

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

-- Obtiene el primer elemento de una lista
cabeza :: [a] -> a
cabeza = L.head

ultimo :: [a] -> a
ultimo = L.last

vacia :: [a] -> Bool
vacia = L.null

obtieneElemento :: [a] -> Int -> a
obtieneElemento lista i = lista !! i

numeroDigitos :: Int -> Int
numeroDigitos n = tamLista $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = tamListaGen $ digitos n

esPrimerElementoCero :: (Eq a, Num a) => [a] -> Bool
esPrimerElementoCero ls
    | cabeza ls==0 = True
    | otherwise = False

--Obtiene el tamaño de una lista
tamLista :: Num a => [a] -> Int
tamLista = L.length

tamListaGen :: Num a => [a] -> Integer
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
obtieneIndice caracter = cabeza [i | (c,i)<-asociaciones, c==caracter]

    {----------------------------------------------------------------------
                    Aritmética modular y números primos
    ----------------------------------------------------------------------}

--Se obtiene el grupo multiplicativo del número p
obtieneGrupoMultiplicativo :: (Integral a, Num a) => a -> [a]
obtieneGrupoMultiplicativo p = P.filter (\x -> gcd x p == 1) [1..p-1]

factores :: (Integral a, Num a) => a -> [a]
factores n
    | n<=1 = []
    | otherwise = factoriza n 2
    where
        factoriza 1 _ = []
        factoriza m c
            | mod m c==0 = c:factoriza (div m c) c
            | otherwise = factoriza m (c+1)

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
  | b == 0 = Tripleta 3 (a, 1, 0)
  | otherwise = Tripleta 3 (d, x, y - div a b * x)
  where
    Tripleta 3 (d, x, y) = euclidesExtendido b (mod a b)

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

    {----------------------------------------------------------------------
                    Funciones auxiliares para los mensajes
    ----------------------------------------------------------------------}

-- Parte en n trozos
parte :: Int -> [a] -> [[a]]
parte _ [] = []
parte n xs = L.take n xs:parte n (L.drop n xs)

deStringAInteger :: String -> Integer
deStringAInteger = read . L.concatMap aux
    where
        aux c | c<'\n' = "00" P.++ show (ord c)
              | c<'d' = '0':show (ord c)
              | otherwise = show (ord c)

deIntegerAString :: Integer -> String
deIntegerAString m
    | x==0 = aux s
    | x==1 = chr (read (L.take 1 s)):aux (L.drop 1 s)
    | x==2 = chr (read (L.take 2 s)):aux (L.drop 2 s)
    where
        s = show m
        x = rem (L.length s) 3
        aux n = L.map (chr . read) (parte 3 n)

    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

-- Introduce dos números primos en una tupla
introduceEnTupla :: (Integral a, Num a) => a -> a -> Tupla a
introduceEnTupla p q = Tupla 2 (p,q)

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