{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module RSA where

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo RSA
    ----------------------------------------------------------------------}

import UtilGeneral
import UtilCripto
import AutomataCelular
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

-- Funciones creadas gracias a "Criptografía desde el punto de vista de la programación funcional" (con modificaciones personales)
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

-- Las siguientes dos funciones fueron creadas gracias a "Criptografía desde el punto de vista de la programación funcional"
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
        --numeroAsociadoACifrado = transformaTextoEnEntero m 
        numeroAsociadoACifrado = read m
        listaOperacionModular = [fromInteger $ exponenciacionModular (toInteger c) d n | c<-numeroAsociadoACifrado]


main = do 
    putStrLn "Introduzca el texto a cifrar:"
    texto <- getLine
    let p1 = 53419 --53419 --100003 --1009 --6070361010663577289 --83   RSA.obtienePrimoAleatorio
    let p2 = 100057 --90073 --100057 --1223 --3209762499797668553 --97  RSA.obtienePrimoAleatorio
    let phiN = calculoPhi (fromInteger p1) (fromInteger p2)
        n   = calculoN (fromInteger p1, fromInteger p2)
        claves = clavesPublicaYPrivada (fromInteger p1, fromInteger p2)
        privada = parPrivado claves 
        publica = parPublico claves
        e = snd publica
        d = snd privada
        mensajeNum = transformaTextoEnEntero texto
        cif = cifradoRSA texto (n,e)
        descif = descifradoRSA cif (n,d)
        menDescif = transformaEnteroEnTexto (read descif)
    imprime ("p1: " ++ show p1 ++ " y p2: " ++ show p2)
    imprime ("Clave pública: " ++ show publica ++ " y clave privada: " ++ show privada)
    imprime "El texto ha sido cifrado."
    imprime ("Texto cifrado: " ++ show cif)
    putStr "El texto se ha descifrado "
    if menDescif == texto then do
        imprime "correctamente."
        imprime ("Texto descifrado: " ++ show descif)
    else do
        imprime "de forma incorrecta."
        imprime ("Texto descifrado: " ++ show descif)
    imprime ("Texto descifrado: " ++ menDescif)

obtienePrimoAleatorio :: IO Integer
obtienePrimoAleatorio = do
    gen <- newStdGen
    let (semillaLista, nuevoGen) = randomR (1, 1000) gen :: (Int, StdGen)
    let (semillaCeldas, genNuevo) = randomR (100, 6005) nuevoGen :: (Int, StdGen)
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        --genera número aleatorio de celdas que tendrá el autómata
    let numCeldas | even numCeldasAlt = numCeldasAlt +1                         --si el número de celdas que se genera de manera aleatoria es par, le suma uno para que sea impar
                  | otherwise = numCeldasAlt
    let listaAleatorios = generaAleatoriosL semillaLista
    let listaAleatoriosBase2 = concat (cambioABase2Lista listaAleatorios)                --se pasa la lista de aleatorios a base 2 y se aplana
    --Se crea el autómata para generar el número pseudoaleatorio
    let inicia = iniciaAC numCeldas listaAleatoriosBase2
    let automata = generaAC numCeldas (regla 30) inicia
    let indices = [1..genericLength automata-1]
    let listaCentros = take 16 [elementoCentral f | f<-automata]
    let num = deListaBinarioANum listaCentros
    let control = esPrimo (toInteger num)
    semillaPrimos <- now
    let opcion = generaAleatorio' semillaPrimos             --obtiene un número aleatorio entre 0 y 1 para elegir de manera aleatoria si se buscará un primo por encima o por debajo del número
    --se genera el número primo a partir del AC
    let p | control = num                                   --si el número ya es primo, no hay que buscarlo
          | opcion == 0 = obtienePrimoCercanoInf num
          | otherwise = obtienePrimoCercanoSup num
    return (toInteger p)





-------------------------------------------------------------------

-- Alternativa poco segura: 

-- cifraMensaje :: Mensaje -> Clave -> (Mensaje, [Int])
-- cifraMensaje msg (e,n) = (deNumerosATexto numero, preparado)
--     where
--         preparado = prepararTexto msg
--         mensaje = toInteger $ transformaEnNumero preparado
--         numero = digitos $ fromInteger $ exponenciacionModular mensaje e n

-- descifraMensaje :: (Mensaje, [Int]) -> Clave -> Mensaje
-- descifraMensaje (msg, control) (e,n) = restaurarTexto control