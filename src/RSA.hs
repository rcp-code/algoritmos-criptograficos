module RSA where

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo RSA
    ----------------------------------------------------------------------}

import UtilGeneral
import UtilCripto
import AutomataCelular
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

claveNE :: (Integer, Integer) -> Int -> Clave
claveNE pq@(p, q) semilla = primero [(toInteger aleatorio, n) | aleatorio<-generaAleatoriosL semilla, sonCoprimos (toInteger aleatorio) phi]
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

compruebaClavePrivada :: ClavePublicaYPrivadaRSA -> Integer -> Bool
compruebaClavePrivada (ClavePublicaYPrivadaRSA e _ d _ _) phi_n = d*e == mod 1 phi_n

compruebaClavePrivada' :: Integer -> Integer -> Integer -> Bool 
compruebaClavePrivada' e d phi_n = d*e == mod 1 phi_n

    {-------------------------------------------------------------------------
                            Clave pública y privada
    -------------------------------------------------------------------------}

-- Funciones creadas gracias a "Criptografía desde el punto de vista de la programación funcional" (con modificaciones personales)
clavesPublicaYPrivadaIO :: (Integer, Integer) -> IO ClavePublicaYPrivadaRSA
clavesPublicaYPrivadaIO pq@(p, q) = do
    semilla <- now
    --let e = unsafePerformIO (generate (calculoE' phi))
    let en = claveNE pq semilla
    let d = exponenentesMod (fst en) (phi-1) phi
    if compruebaClavePrivada' (fst en) d phi then do
        let priv = construyeClave d (snd en)
        let cpp = ClavePublicaYPrivadaRSA {e=fst en, n=snd en, d=d, parPublico=en, parPrivado=priv}
        imprime "Se han generado con éxito las claves pública y privada."
        return cpp
    else do
       clavesPublicaYPrivadaIO pq
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

cifraRSA :: Mensaje -> Clave -> Mensaje
cifraRSA m (n,e) = transformaEnteroEnTexto cifrado
    where 
        numeroAsociado = transformaListaNumerosEnNumero $ transformaTextoEnEntero m 
        operacionMod = exponenentesMod (toInteger numeroAsociado) e n
        parteEn2 = parte 2 cifrado
        cifrado = transformaListaNumeros parteEn2

descifraRSA :: Mensaje -> Clave -> Mensaje
descifraRSA = cifraRSA







-- cifraRSA :: Mensaje -> Clave -> Integer
-- cifraRSA m (e,n) = exponenentesMod (toInteger entero) e n
--     where 
--         binario = traduceTextoABinario' m 
--         entero = deBitsAInt binario

-- descifraRSA :: Integer -> Clave -> Mensaje
-- descifraRSA m (d,n) = binarioATexto desencriptado
--     where 
--         numero = exponenentesMod m d n
--         desencriptado = deIntABits $ fromInteger numero

-------------------------------------------------------------------

-- cifraMensaje :: Mensaje -> Clave -> Mensaje
-- cifraMensaje msg (e,n) = deNumerosATexto numero
--     where
--         preparado = prepararTexto msg
--         mensaje = toInteger $ transformaEnNumero preparado
--         numero = digitos $ fromInteger $ exponenentesMod mensaje e n

-- descifraMensaje :: Mensaje -> Clave -> Mensaje
-- descifraMensaje msg clave@(e,n) = restaurarTexto numero
--     where
--         preparado = prepararTexto msg
--         mensaje = toInteger $ transformaEnNumero preparado
--         numero = digitos $ fromInteger $ exponenentesMod mensaje e n
        
-------------------------------------------------------------------

-- Alternativa poco segura: 

-- cifraMensaje :: Mensaje -> Clave -> (Mensaje, [Int])
-- cifraMensaje msg (e,n) = (deNumerosATexto numero, preparado)
--     where
--         preparado = prepararTexto msg
--         mensaje = toInteger $ transformaEnNumero preparado
--         numero = digitos $ fromInteger $ exponenentesMod mensaje e n

-- descifraMensaje :: (Mensaje, [Int]) -> Clave -> Mensaje
-- descifraMensaje (msg, control) (e,n) = restaurarTexto control