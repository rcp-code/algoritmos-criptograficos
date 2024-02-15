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

    {----------------------------------------------------------------------
                            Comprobaciones
    ----------------------------------------------------------------------}

-- Comprueba si un número es entero
esEntero :: (RealFrac a) => a -> Bool
esEntero x = snd (properFraction x) == 0

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
cabeza :: [a] -> a
cabeza = L.head

-- Obtiene el último elemento de una lista
ultimo :: [a] -> a
ultimo = L.last

-- Comprueba si la lista está vacía
esVacia :: [a] -> Bool
esVacia = L.null

obtieneElemento :: [a] -> Int -> a
obtieneElemento lista i = lista !! i

numeroDigitos :: Int -> Int
numeroDigitos n = tamLista $ digitos n

numeroDigitos' :: Int -> Integer
numeroDigitos' n = tamListaGen $ digitos n

elementoCentral :: [Int] -> Int
elementoCentral lista = lista !! pos
  where 
    pos = abs $ div (tamLista lista) 2

esPrimerElementoCero :: (Eq a, Num a) => [a] -> Bool
esPrimerElementoCero ls
    | cabeza ls==0 = True
    | otherwise = False

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
obtieneIndice caracter = cabeza [i | (c,i)<-asociaciones, c==caracter]

convierteAVector :: [a] -> Vector a
convierteAVector = V.fromList

-- Obtiene un vector de un vector de vectores
obtieneVector :: Int -> Vector (Vector a) -> Vector a
obtieneVector i vector = vector V.! i

-- obtiene el primero elemento de un vector
cabezaVector :: Vector a -> a
cabezaVector = V.head

-- obtiene el último elemento de un vector
ultimoVector :: Vector a -> a
ultimoVector = V.last

-- Vector vacío
vectorVacio :: Vector a
vectorVacio = V.empty

-- Agrega un elemento al principio de un vector
agregaPrimero :: a -> Vector a -> Vector a
agregaPrimero = V.cons

-- Agrega un elemento al final de un vector
agregaUltimo :: Vector a -> a -> Vector a
agregaUltimo = V.snoc

-- Agrega un vector al principio de un vector de vectores
agregaVectorPrimero :: Vector a -> Vector (Vector a) -> Vector (Vector a)
agregaVectorPrimero = V.cons

-- Agrega un vector al final de un vector de vectores
agregaVectorUltimo :: Vector (Vector a) -> Vector a -> Vector (Vector a)
agregaVectorUltimo = V.snoc

    {----------------------------------------------------------------------
                    Funciones auxiliares para los mensajes
    ----------------------------------------------------------------------}

-- Parte en n trozos
parte :: Int -> [a] -> [[a]]
parte _ [] = []
parte n xs = L.take n xs:parte n (L.drop n xs)

subvectores :: Vector a -> [a]
subvectores vector = subvectores' 0 vector []

subvectores' :: Int -> Vector a -> [a] -> [a]
subvectores' pos v aux
    | pos>=0 && pos<=V.length v-3 = subvectores' (pos+1) v (aux L.++ V.toList (V.slice pos 3 v))
    | otherwise = aux

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

-- Otra forma de convertir una lista de números a una lista de caracteres
deNumerosATexto' :: [Int] -> [Char]
deNumerosATexto' [] = []
deNumerosATexto' (x:xs) =
  let dosDigitos = read (show x L.++ L.take 1 (L.map intToDigit xs)) :: Int
  in if dosDigitos <= L.genericLength caracteres - 1 then 
        deNumeroACaracter dosDigitos L.++ deNumerosATexto' (L.drop 1 xs)
     else deNumeroACaracter x L.++ deNumerosATexto' xs

{- Esta versión verifica si el número actual y el siguiente pueden formar un número de dos dígitos válido. 
    Si es así, se usa ese número de dos dígitos y se avanza en la lista de números saltando el siguiente elemento. 
    De lo contrario, se utiliza el número actual solo y se avanza hacia el siguiente elemento. -}

-- Función para restaurar el texto después de descifrar
restaurarTexto :: [Int] -> Mensaje
restaurarTexto = deNumerosATexto'

-- preparaMensaje :: Mensaje -> [Integer]
-- preparaMensaje "" = []
-- preparaMensaje ms = preparaMensaje' ms []

-- preparaMensaje' :: Mensaje -> [Integer] -> [Integer]
-- preparaMensaje' "" auxs = auxs
-- preparaMensaje' (c:ms) auxs
--     | c `L.notElem` caracteres = error "Caracter invalido en el mensaje."
--     | otherwise = preparaMensaje' ms (auxs P.++ listaTransf)
--     where
--         indice = obtieneIndice c
--         elemento = asociaciones !! indice
--         numero = snd elemento
--         listaTransf = introduceEnLista numero

-- Función auxiliar que transforma un número en una lista con sus dígitos de forma que:
--      · Si el número tiene menos de 2 cifras, le agrega un 0 a la izquierda.
--      · En caso contrario, parte el número en 2 dígitos (en principio, y por ahora, no habrá números con más de dos dígitos)
introduceEnLista :: Int -> [Integer]
introduceEnLista n
    | numeroDigitos n<2 = 0:[toInteger n]
    | otherwise = [toInteger (deStringAInt s) | s<-ns]
    where
        numero = toInteger n
        ns = parte 1 (show numero)

-- transformaEnNumero :: (Integral a, Num a) => [a] -> a
-- transformaEnNumero [] = error "No se puede convertir a entero porque la lista esta esVacia."
-- transformaEnNumero ns = deDigitosANum ns

transformaEnNumero :: [Int] -> Int
transformaEnNumero [] = error "No se puede convertir a entero porque la lista esta esVacia."
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



-- deIntegerAString :: Integer -> String
-- deIntegerAString m
--     | x==0 = aux s
--     | x==1 = chr (read (L.take 1 s)):aux (L.drop 1 s)
--     | x==2 = chr (read (L.take 2 s)):aux (L.drop 2 s)
--     where
--         s = show m
--         x = rem (L.length s) 3
--         aux n = L.map (chr . read) (parte 3 n)

    {----------------------------------------------------------------------
                                Otras funciones
    ----------------------------------------------------------------------}

-- Introduce dos números en una tupla
introduceEnTupla :: (Integral a, Num a) => a -> a -> (a, a)
introduceEnTupla p q = (p,q)

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