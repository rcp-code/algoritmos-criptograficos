module RSA where

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo RSA
    ----------------------------------------------------------------------}

import UtilGeneral
import UtilCripto
import AutomataCelular1D
import Tipos

import Data.List as L
import Test.QuickCheck.Property
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Data.Char
import UtilIO



calculoPhi :: Integer -> Integer -> Integer
calculoPhi p q = abs (p-1)*(q-1)


    {-------------------------------------------------------------------------
                                Clave pública
    -------------------------------------------------------------------------}

calculoN :: (Integer, Integer) -> Integer
calculoN (p, q) = abs (p*q)

claveNE :: (Integer, Integer) -> Integer -> Int -> Clave
claveNE pq@(p, q) e semilla = cabeza [(toInteger aleatorio, n) | aleatorio<-generaAleatoriosL semilla, sonCoprimos (toInteger aleatorio) phi]
    where
        phi = calculoPhi p q
        n = calculoN pq

-- clavePublica :: Integer -> Integer -> Clave
-- clavePublica e phi_n
--     | compruebaE e phi_n = construyeClave e phi_n
--     | otherwise = error "El exponente de la clave publica no es valido."

-- Otra versión: extraído (este y el siguiente) de "Criptografía desde el punto de vista de la programación funcional"
calculoE :: Integer -> Integer
calculoE phiN = unsafePerformIO (generate (calculoE' phiN))

calculoE' :: Integer -> Gen Integer                             --Gen: generador de valores de tipo Int (generalización: Gen a)
calculoE' phiN = suchThat (choose (1,phiN)) (sonCoprimos phiN)  --se genera un valor aleatorio que satisfaga el predicado (en este caso, que sean coprimos)
                                                                --choose: genera un número aleatorio entre 1 y phiN

    {-------------------------------------------------------------------------
                                Clave privada
    -------------------------------------------------------------------------}

-- compruebaClavePrivada :: Integer -> Integer -> Integer -> Bool
-- compruebaClavePrivada d e phi_n = d*e == mod 1 phi_n

compruebaClavePrivada :: ClavePublicaYPrivadaRSA -> Integer -> Bool
compruebaClavePrivada (ClavePublicaYPrivadaRSA e _ d _ _) phi_n = d*e == mod 1 phi_n

    {-------------------------------------------------------------------------
                            Clave pública y privada
    -------------------------------------------------------------------------}

-- Funciones creadas gracias a "Criptografía desde el punto de vista de la programación funcional" (con modificaciones personales)
clavesPublicaYPrivadaIO :: (Integer, Integer) -> IO ClavePublicaYPrivadaRSA
clavesPublicaYPrivadaIO pq@(p, q) = do
    semilla <- now
    let e = unsafePerformIO (generate (calculoE' phi))
    let d = exponenentesMod e (phi-1) phi
    let clavePub = claveNE pq e semilla
    let clavePriv = construyeClave d n
    let cpp = ClavePublicaYPrivadaRSA {e=e, n=n, d=d, parPublico=clavePub, parPrivado=clavePriv}
    return cpp
    where
        n = calculoN pq
        phi = calculoPhi p q

clavesPublicaYPrivada :: (Integer, Integer) -> ClavePublicaYPrivadaRSA
clavesPublicaYPrivada pq = unsafePerformIO (clavesPublicaYPrivadaIO pq)

    {-------------------------------------------------------------------------
                            Cifrado y descifrado
    -------------------------------------------------------------------------}

-- Las siguientes dos funciones fueron creadas gracias a "Criptografía desde el punto de vista de la programación funcional"
exponenentesMod :: Integer -> Integer -> Integer -> Integer
exponenentesMod c 1 n = mod c n
exponenentesMod c e n
    | even e = exponenentesMod m de n
    | otherwise = mod exp n
    where
        m = mod (c*c) n
        de = div e 2
        exp = c*exponenentesMod c (e-1) n

prop_ExpMod :: Integer -> Integer -> Integer -> Property
prop_ExpMod c e n = e>0 && n>0 ==> exponenentesMod c e n == mod exp n
    where
        exp = c^e

cifraMensaje :: Mensaje -> Clave -> (Mensaje, [Int])
cifraMensaje msg (e,n) = (deNumerosATexto numero, preparado)
    where
        preparado = prepararTexto msg
        mensaje = toInteger $ transformaEnNumero preparado
        numero = digitos $ fromInteger $ exponenentesMod mensaje e n

descifraMensaje :: (Mensaje, [Int]) -> Clave -> Mensaje
descifraMensaje (msg, control) (e,n) = restaurarTexto control

-- cifraMensaje :: String -> Clave -> (String, Bool) 
-- cifraMensaje m (n,e) = (deIntegerAString (exponenentesMod (transformaEnNumero preparado) e n), control)
--     where 
--         preparado = preparaMensaje m
--         control = esPrimerElementoCero preparado

-- descifraMensaje :: (String, Bool) -> Clave -> String 
-- descifraMensaje = cifraMensaje

-- cifraMensaje :: ClavePublicaYPrivadaRSA -> Mensaje -> (Integer, Bool)
-- cifraMensaje (ClavePublicaYPrivadaRSA e n _ _ _) msg = (cifrado, control)
--     where
--         preparado = preparaMensaje msg
--         control = esPrimerElementoCero preparado
--         cifrado = exponenentesMod (toInteger $ deDigitosANum preparado) e n

-- descifraMensaje :: ClavePublicaYPrivadaRSA -> (Integer, Bool) -> Mensaje
-- descifraMensaje clave@(ClavePublicaYPrivadaRSA e n d _ _) (cif,con) = deRepresentacion (fromInteger descifrado) con
--     where
--         descifrado = exponenentesMod cif e n
