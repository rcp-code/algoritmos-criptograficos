module Interaccion where

import AC
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
    imprime "Fin de la ejecución."

principal :: IO ()
principal = do
    imprime "Advertencia: el programa puede tardar unos segundos en encontrar los primos."
    algoritmoRSA
    -- imprime "¿Qué algoritmo desea aplicar: RSA, ElGamal o Intercambio de claves? "
    -- opcion <- leeMensaje
    -- if opcion == "RSA" || opcion=="rsa" then do
    --     imprime "Se está ejecutando el algoritmo RSA..."
    --     algoritmoRSA
    -- else if opcion == "XXX" || opcion=="xxx" then do
    --     imprime "Se está ejecutando otro algoritmo..."
    -- else do
    --     imprime "No se ha seleccionado una opción válida."
    imprime "Fin de la ejecución de principal."

obtienePrimoAleatorio :: IO Int
obtienePrimoAleatorio = do
    --Se crea el autómata para generar el número pseudoaleatorio
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        --genera número aleatorio de celdas que tendrá el autómata
    let numCeldas | even numCeldasAlt = numCeldasAlt +1                         --si el número de celdas que se genera de manera aleatoria es par, le suma uno para que sea impar
                  | otherwise = numCeldasAlt
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)                --se pasa la lista de aleatorios a base 2 y se aplana
    let inicia = iniciaAC numCeldas lAleatBase2
    let automata = generaAC numCeldas (regla 30) inicia
    imprime "El tamaño del autómata es: "
    imprime $ show $ L.genericLength automata
    imprime "La mitad es: "
    imprime $ show (div (L.genericLength automata) 2)
    let indices = [1..L.genericLength automata-1]
    let listaCentros = L.concat [c | (c,i)<-zip automata indices, i==div (L.genericLength automata) 2]
    imprime "La lista de centros es: "
    imprime $ show listaCentros
    let num = deListaBinarioANum listaCentros
    imprime "Número obtenido:"
    imprime $ show num
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
    let tuplaPrimos = introduceEnTupla (toInteger p) (toInteger q)
    putStr "Los primos p y q son los siguientes: "
    imprime $ show tuplaPrimos
    let n = calculoN tuplaPrimos
    semillaE <- now
    let phiN = calculoPhi (toInteger p) (toInteger q)
    let clavesPubYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePub = parPublico clavesPubYPriv               --obtiene la clave pública
    let clavePriv = parPrivado clavesPubYPriv              --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePub
    putStr "La clave privada es: "
    imprime $ show clavePriv
    -- imprime "Introduce el mensaje que se va a cifrar:"
    -- msg <- leeMensaje
    -- imprime "El mensaje que va a cifrarse es el siguiente:"
    -- imprime msg
    -- let preparado = deDigitosAInt (preparaMensaje msg)
    -- imprime "El mensaje preparado es el siguiente: "
    -- imprime $ show preparado
    -- let mensajeCifrado = cifraMensaje clavesPubYPriv msg
    -- imprime "El mensaje se ha cifrado correctamente de la siguiente manera:"
    -- imprime $ show (fst mensajeCifrado)
    -- imprime "Se va a proceder con el descifrado del mensaje..."
    -- let mensajeOriginal = descifraMensaje clavesPubYPriv mensajeCifrado
    -- imprime "El mensaje se ha descifrado y es el siguiente: "
    -- imprime mensajeOriginal



-- Obtiene un número aleatorio como semilla:
obtieneNumAleatorio :: IO Int
obtieneNumAleatorio = do
    --Se crea el autómata para generar el número pseudoaleatorio
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        
    let numCeldas | even numCeldasAlt = numCeldasAlt +1                         
                  | otherwise = numCeldasAlt
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)
    let inicia = iniciaAC numCeldas lAleatBase2
    let automata = generaAC numCeldas (regla 30) inicia
    let indices = [1..L.genericLength automata-1]
    let listaCentros = L.concat [c | (c,i)<-zip automata indices, i==div (L.genericLength automata) 2]
    let num = deListaBinarioANum listaCentros
    putStr "Número aleatorio seleccionado: "
    imprime $ show num
    return num

    {----------------------------------------------------------------------
                                    Pruebas
    ----------------------------------------------------------------------}

pruebaAutomata :: IO ()
pruebaAutomata = do
    imprime "Selecciona una regla para el autómata celular: "
    r <- leeMensaje
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        
    let numCeldas | even numCeldasAlt = numCeldasAlt +1                         
                  | otherwise = numCeldasAlt
    let m0 = "El total de celdas es: " ++ show numCeldas
    imprime m0
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)
    let inicia = iniciaAC numCeldas lAleatBase2
    let automata = generaAC numCeldas (regla (deStringAInt r)) inicia
    muestraAC numCeldas (regla (deStringAInt r)) inicia