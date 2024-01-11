module RSA where

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo RSA
    ----------------------------------------------------------------------}

import Util
import AC
import Tipos

import Data.List as L
import Test.QuickCheck.Property
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Data.Char



calculoPhi :: Integer -> Integer -> Integer
calculoPhi p q = abs (p-1)*(q-1)


    {-------------------------------------------------------------------------
                                Clave pública
    -------------------------------------------------------------------------}

    {- Primero: dos números primos lo suficientemente grandes (p y q).
       Segundo: calcular la primera parte de la clave pública: n=p*q.
       Tercero: calcular la segunda parte, el exponente e, que debe ser entero y primo con n.-}

calculoN :: Tupla Integer -> Integer
calculoN (Tupla 2 (p, q)) = abs (p*q)

compruebaE :: Integer -> Integer -> Bool
compruebaE e phi_n = gcd e phi_n == 1

clavePublica :: Integer -> Integer -> Clave
clavePublica e phi_n
    | compruebaE e phi_n = construyeClave e phi_n 
    | otherwise = error "El exponente de la clave pública no es válido."

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

compruebaClavePrivada :: ClavePublicaYPrivada -> Integer -> Bool
compruebaClavePrivada (ClavePublicaYPrivada e _ d _ _) phi_n = d*e == mod 1 phi_n

    {-------------------------------------------------------------------------
                            Clave pública y privada
    -------------------------------------------------------------------------}

-- Otra versión (unión claves pública y privada): extraídos (este y el siguiente) de "Criptografía desde el punto de vista de la programación funcional",
-- pero con modificaciones personales
clavesPublicaYPrivadaIO :: Integer -> Integer -> IO ClavePublicaYPrivada
clavesPublicaYPrivadaIO n phiN = do
    let e = unsafePerformIO (generate (calculoE' phiN))
    let d = inverso e phiN 
    let clavePub = clavePublica n e
    let clavePriv = construyeClave n d
    let cpp = ClavePublicaYPrivada {e=e, n=n, d=d, publica=clavePub, privada=clavePriv}
    return cpp

clavesPublicaYPrivada :: Integer -> Integer -> ClavePublicaYPrivada
clavesPublicaYPrivada n phiN = unsafePerformIO (clavesPublicaYPrivadaIO n phiN)

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

cifraMensaje :: ClavePublicaYPrivada -> [Integer] -> Mensaje
cifraMensaje (ClavePublicaYPrivada e n _ _ _) msg = show $ exponenentesMod (transformaEnEntero msg) e n

cifraMensaje' :: ClavePublicaYPrivada -> Mensaje -> Mensaje
cifraMensaje' (ClavePublicaYPrivada e n d _ _) msg
    | conteoLetras==length msg = show $ exponenentesMod (toInteger (deStringAInt msg)) e n
    | otherwise = show $ exponenentesMod (toInteger (deStringAInt msg)) d n
    where 
        conteoLetras = sum [1 | a<-msg, isPrint a]

descifraMensaje :: ClavePublicaYPrivada -> [Integer] -> Mensaje
descifraMensaje clave@(ClavePublicaYPrivada e n d _ _) = cifraMensaje clave