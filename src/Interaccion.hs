{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Interaccion where

import AC
import IntercambioClaves
import ElGamal
import RSA
import Util
import UtilIO
import Tipos
---
import Data.List as L
import System.Random
import System.IO

mainInteraccion :: IO ()
mainInteraccion = do
    imprime "Cargando..."
    principal
    imprime "¿Desea volver a ejecutar el programa? (S/N)"
    resp <- leeChar
    if resp == 'S' then do
        principal
    else do
        imprime "Fin de la ejecución."

principal :: IO ()
principal = do
    imprime "Advertencia: el programa puede tardar unos segundos en encontrar los primos."
    imprime "¿Qué algoritmo desea aplicar: RSA, ElGamal o Intercambio de claves? "
    opcion <- leeMensaje
    if opcion == "RSA" || opcion=="rsa" then do
        imprime "Se está ejecutando el algoritmo RSA..."
        algoritmoRSA
    else if opcion == "ElGamal" || opcion=="elgamal" then do
        imprime "Se está ejecutando el algoritmo ElGamal..."
        algoritmoElGamal
    else if opcion == "Intercambio de claves" || opcion == "intercambio claves" then do
        imprime "Se está ejecutando el algoritmo de Intercambio de claves..."
        algoritmoIntercambioClaves
    else do
        imprime "No se ha seleccionado una opción válida."
    imprime "Fin de la ejecución de principal."

obtienePrimoAleatorio :: IO Int
obtienePrimoAleatorio = do
    --Se crea el autómata para generar el número pseudoaleatorio
    semillaLista <- now
    semillaCelulas <- now
    let numCelulas = generaAleatorio semillaCelulas minCelulas maxCelulas        --genera número aleatorio de células que tendrá el autómata
    --let m0 = "El total de células es: " ++ show numCelulas
    --imprime m0
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambiaListaIntABase2 listaAleatorios)              --se pasa la lista de aleatorios a base 2 y se aplana
    let inicia = iniciaAC numCelulas lAleatBase2
    let automata = generaAC numCelulas (regla 30) inicia
    let indices = [1..L.genericLength automata-1]
    semillaAutomata1 <- now
    let ind = generaAleatorio semillaAutomata1 1 (L.genericLength indices)
    let listaCelulas = automata !! ind
    let num = deListaBinarioAInt listaCelulas
    let control = esPrimo (toInteger num)
    semillaPrimos <- now
    let opcion = generaAleatorio' semillaPrimos             --obtiene un número aleatorio entre 0 y 1 para elegir de manera aleatoria si se buscará un primo por encima o por debajo del número
    --se genera el número primo a partir del AC
    let p | control = num                                   --si el número ya es primo, no hay que buscarlo
          | opcion == 0 = obtienePrimoCercanoInf num
          | otherwise = obtienePrimoCercanoSup num
    let mensaje = "El número primo obtenido es: " ++ show p
    imprime mensaje
    return p

algoritmoRSA :: IO ()
algoritmoRSA = do
    p <- obtienePrimoAleatorio
    q <- obtienePrimoAleatorio
    let tuplaPrimos = introducePrimosEnTupla (toInteger p) (toInteger q)
    putStr "Los primos p y q son los siguientes: "
    imprime $ show tuplaPrimos
    let n = calculoN tuplaPrimos
    semillaE <- now
    let phiN = calculoPhi (toInteger p) (toInteger q)
    let clavesPubYPriv = clavesPublicaYPrivada n phiN
    let clavePub = publica clavesPubYPriv               --obtiene la clave pública
    let clavePriv = privada clavesPubYPriv              --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePub
    putStr "La clave privada es: "
    imprime $ show clavePriv
    imprime "Introduce el mensaje que se va a cifrar:"
    msg <- leeMensaje
    imprime "El mensaje que va a cifrarse es el siguiente:"
    imprime msg
    let preparado = preparaMensaje msg
    imprime "El mensaje preparado es el siguiente: "
    imprime $ show preparado
    let mensajeCifrado = cifraMensaje clavesPubYPriv preparado
    imprime "El mensaje se ha cifrado correctamente."
    imprime $ show mensajeCifrado
    imprime "Se va a proceder con el descifrado del mensaje..."
    -- let mensajeOriginal = descifraMensaje clavesPubYPriv mensajeCifrado
    -- imprime "El mensaje original en formato numérico es el siguiente: "
    -- imprime $ show mensajeOriginal
    -- let msgOrig = read mensajeOriginal
    -- imprime "El mensaje se ha descifrado. Comprueba si este es el mensaje original:"
    -- imprime msgOrig

algoritmoIntercambioClaves :: IO ()
algoritmoIntercambioClaves = do
    p <- obtienePrimoAleatorio
    semillaGenA1 <- now
    --semillaGenA2 <- now
    --let genA = generador semillaGenA1 semillaGenA2 p
    let genA = generador semillaGenA1 (toInteger p)
    putStr "El generador de A es: "
    imprime $ show genA
    semillaGenB1 <- now
    --semillaGenB2 <- now
    --let genB = generador semillaGenB1 semillaGenA2 p
    let genB = generador semillaGenB1 (toInteger p)
    putStr "El generador de B es: "
    imprime $ show genB
    semillaSecretoA <- now
    let secretoA = eligeSecreto semillaSecretoA genA
    semillaSecretoB <- now
    let secretoB = eligeSecreto semillaSecretoB genB
    putStr "El secreto de A es: "
    imprime $ show secretoA
    putStr "El secreto de B es: "
    imprime $ show secretoB
    let claveComp = calculaClaveCompartida genA secretoA secretoB
    let claveComp' = calculaClaveCompartida genA secretoA secretoB
    let comp = compruebaClaveCompartida claveComp claveComp'
    if comp then do
        putStr "La clave compartida es "
        hFlush stdout
        imprime $ show claveComp
    else do
        imprime "La clave compartida no es válida."

-- Otra versión (genera las semillas con AC)
-- algoritmoIntercambioClaves :: IO ()
-- algoritmoIntercambioClaves = do
--     p <- obtienePrimoAleatorio
--     semillaGenA1 <- obtieneNumAleatorio
--     semillaGenA2 <- obtieneNumAleatorio
--     let genA = generador semillaGenA1 semillaGenA2 p
--     let genA = generador semillaGenA1 semillaGenA2 p
--     putStr "El generador de A es: "
--     imprime $ show genA
--     semillaGenB <- now
--     semillaGenB1 <- obtieneNumAleatorio
--     semillaGenB2 <- obtieneNumAleatorio
--     let genB = generador semillaGenB1 semillaGenB2 p
--     putStr "El generador de B es: "
--     imprime $ show genB
--     semillaSecretoA <- obtieneNumAleatorio
--     putStr "La semilla para el secreto A es: "
--     imprime $ show semillaSecretoA
--     let secretoA = eligeSecreto semillaSecretoA genA
--     semillaSecretoB <- obtieneNumAleatorio
--     putStr "La semilla para el secreto B es: "
--     imprime $ show semillaSecretoB
--     let secretoB = eligeSecreto semillaSecretoB genB
--     putStr "El secreto de A es: "
--     imprime $ show secretoA
--     putStr "El secreto de B es: "
--     imprime $ show secretoB
--     let claveComp = calculaClaveCompartida p (snd genA) secretoA secretoB
--     let claveComp' = calculaClaveCompartida p (snd genA) secretoA secretoB
--     let comp = compruebaClaveCompartida claveComp claveComp'
--     if comp then do
--         putStr "La clave compartida es "
--         imprime $ show claveComp
--     else do
--         imprime "La clave compartida no es válida."


algoritmoElGamal :: IO ()
algoritmoElGamal = do
    p <- obtienePrimoAleatorio

    imprime ""


-- Obtiene un número aleatorio como semilla:
obtieneNumAleatorio :: IO Int
obtieneNumAleatorio = do
    --Se crea el autómata para generar el número pseudoaleatorio
    semillaLista <- now
    semillaCelulas <- now
    let numCelulas = generaAleatorio semillaCelulas minCelulas maxCelulas        --genera número aleatorio de células que tendrá el autómata
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambiaListaIntABase2 listaAleatorios)              --se pasa la lista de aleatorios a base 2 y se aplana
    let inicia = iniciaAC numCelulas lAleatBase2
    let automata = generaAC numCelulas (regla 30) inicia
    let indices = [1..L.genericLength automata-1]
    semillaAutomata1 <- now
    let ind = generaAleatorio semillaAutomata1 1 (L.genericLength indices)
    let listaCelulas = take 5 $ automata !! ind
    putStr "Lista de células con 5 dígitos: "
    imprime $ show listaCelulas
    let num = deListaBinarioAInt listaCelulas
    putStr "Número aleatorio seleccionado: "
    imprime $ show num
    return num

    {----------------------------------------------------------------------
                                    Pruebas
    ----------------------------------------------------------------------}

pruebaAutomata :: IO ()
pruebaAutomata = do
    semillaLista <- now
    semillaCelulas <- now
    let numCelulas = generaAleatorio semillaCelulas minCelulas maxCelulas   --genera número aleatorio de células que tendrá el autómata
    let m0 = "El total de células es: " ++ show numCelulas
    imprime m0
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambiaListaIntABase2 listaAleatorios)         --se pasa la lista de aleatorios a base 2 y se aplana
    let inicia = iniciaAC numCelulas lAleatBase2
    let automata = generaAC numCelulas (regla 30) inicia
    muestraAC numCelulas (regla 30) inicia