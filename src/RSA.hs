{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module RSA where

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo RSA
    ----------------------------------------------------------------------}

import UtilGeneral
import UtilCripto
import AutomataCelular as AC
import Tipos
import Constantes

import Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import Data.Char
import UtilIO
import Test.QuickCheck
import System.Random



calculoPhi :: Integer -> Integer -> Integer
calculoPhi p q = abs (p-1)*(q-1)


    {-------------------------------------------------------------------------
                                Clave pública
    -------------------------------------------------------------------------}

calculoN :: (Integer, Integer) -> Integer
calculoN (p, q) = abs (p*q)

claveNE :: (Integer, Integer) -> Int -> Clave
claveNE pq@(p, q) semilla = primero [(n, toInteger aleatorio) | aleatorio<-generaAleatoriosL semilla, sonCoprimos aleatorio (fromInteger phi)]
    where
        phi = calculoPhi p q
        n = calculoN pq

-- Otra versión: extraído (este y el siguiente) de "Criptografía desde el punto de vista de la programación funcional"
calculoE :: Integer -> Integer
calculoE phiN = unsafePerformIO (generate (calculoE' phiN))

calculoE' :: Integer -> Gen Integer                             --Gen: generador de valores de tipo Int (generalización: Gen a)
calculoE' phiN = suchThat (choose (1,phiN)) (sonCoprimos phiN)  --se genera un valor aleatorio que satisfaga el predicado (en este caso, que sean coprimos)
                                                                --choose: genera un número aleatorio entre 1 y phiN

    {-------------------------------------------------------------------------
                            Clave pública y privada
    -------------------------------------------------------------------------}

-- Creación de las claves pública y privada
clavesPublicaYPrivadaIO :: (Integer, Integer) -> IO ClavePublicaYPrivadaRSA
clavesPublicaYPrivadaIO pq@(p, q) = do
    semilla <- now
    let e = abs $ calculoE phiN
    let d = abs $ inversoModular' e phiN
    let pub = construyeClave n e
    let priv = construyeClave n d
    let cpp = ClavePublicaYPrivadaRSA {e=e, n=n, d=d, parPublico=pub, parPrivado=priv}
    return cpp
    where
        n = calculoN pq
        phiN = calculoPhi p q

clavesPublicaYPrivada :: (Integer, Integer) -> ClavePublicaYPrivadaRSA
clavesPublicaYPrivada pq = unsafePerformIO (clavesPublicaYPrivadaIO pq)

    {-------------------------------------------------------------------------
                            Cifrado y descifrado
    -------------------------------------------------------------------------}

exponenciacionModular :: Integer -> Integer -> Integer -> Integer
exponenciacionModular c 1 n = mod c n
exponenciacionModular c e n
    | even e = exponenciacionModular m de n
    | otherwise = mod exp n
    where
        m = mod (c*c) n
        de = div e 2
        exp = c*exponenciacionModular c (e-1) n

cifradoRSA :: Mensaje -> Clave -> Mensaje
cifradoRSA m (n,e) = show listaOperacionModular
    where
        numeroAsociadoAMensaje = transformaTextoEnEntero m
        listaOperacionModular = [fromInteger $ exponenciacionModular (toInteger c) e n | c<-numeroAsociadoAMensaje]

descifradoRSA :: Mensaje -> Clave -> Mensaje
descifradoRSA m (n,d) = show listaOperacionModular
    where
        numeroAsociadoACifrado = read m
        listaOperacionModular = [fromInteger $ exponenciacionModular (toInteger c) d n | c<-numeroAsociadoACifrado]


main = do
    putStrLn "Introduzca el texto a cifrar:"
    texto <- getLine
    let p1 = unsafePerformIO AC.obtienePrimoAleatorio 
    let p2 = unsafePerformIO AC.obtienePrimoAleatorio
    let phiN = calculoPhi (fromInteger p1) (fromInteger p2)
    let n   = calculoN (fromInteger p1, fromInteger p2)
    let claves = clavesPublicaYPrivada (fromInteger p1, fromInteger p2)
    let privada = parPrivado claves
    let publica = parPublico claves
    let e = snd publica
    let d = snd privada
    let cif = cifradoRSA texto (n,e)
    let descif = descifradoRSA cif (n,d)
    let menDescif = transformaEnteroEnTexto (read descif)
    imprime ("Clave pública: " ++ show publica) -- ++ " y clave privada: " ++ show privada)
    imprime "El texto ha sido cifrado."
    putStr "El texto se ha descifrado "
    if menDescif == texto then do
        imprime "correctamente."
        imprime ("Texto descifrado: " ++ show menDescif)
    else do
        imprime "de forma incorrecta."
        imprime ("Texto descifrado: " ++ show menDescif)


