module Experimentos where

import AutomataCelular
import RSA
import UtilGeneral
import UtilCripto
import UtilIO
import Tipos
import Constantes
import CifradoBloque
import CifradoWolfram
---
import Data.List
import System.Random
import System.IO
import GHC.IO (unsafePerformIO)

mainExperimentos :: IO ()
mainExperimentos = do
    imprime "Cargando..."
    principal
    imprime "Fin del programa."

principal :: IO ()
principal = do
    imprime "Advertencia: el programa puede tardar unos segundos en generar los primos y en realizar los procesos de cifrado y descifrado mediante autómatas celulares."
    imprime "¿Qué algoritmo desea aplicar: RSA (rsa), Cifrado de bloques (bloque) o Cifrado de Wolfram (wolfram)? "
    opcion <- leeMensaje
    if opcion == "RSA" || opcion=="rsa" then do
        imprime "Se está ejecutando RSA..."
        algoritmoRSA
        --algoritmoRSATradicional
    else if opcion == "Cifrado de bloques" || opcion=="bloque" then do
        imprime "Elija la opción preferida de ejecución para el algoritmo de cifrado de bloques basado en AC: (1) versión simplificada o (2) versión segura..."
        version <- leeMensaje
        if version == "1" then do
            imprime "Se está ejecutando la versión simplificada del Cifrado de bloques basado en AC de segundo orden..."
            cifradoBloqueACSOSimplificado
        else do
            imprime "Se está ejecutando Cifrado de bloques basado en AC de segundo orden..."
            cifradoBloqueACSO
    else if opcion == "Cifrado de Wolfram" || opcion=="wolfram" then do
        imprime "Se está ejecutando el cifrado de Wolfram..."
        descifrado <- cifradoWolframIO
        imprime ("El texto descifrado es: " ++ descifrado)
    else do
        imprime "No se ha seleccionado una opción válida."
    imprime "Fin de la ejecución de principal."

algoritmoRSA :: IO ()
algoritmoRSA = do
    p <- AutomataCelular.obtienePrimoAleatorio
    q <- AutomataCelular.obtienePrimoAleatorio
    imprime "Los primos se han generado correctamente."
    let tuplaPrimos = introduceEnTupla p q
    let n = calculoN tuplaPrimos
    let phiN = calculoPhi p q
    let clavesPublicaYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePublica = parPublico clavesPublicaYPriv                --obtiene la clave pública
    let clavePrivada = parPrivado clavesPublicaYPriv                --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePublica
    imprime "Introduce el mensaje que se va a cifrar:"
    msg <- leeMensaje
    imprime "El mensaje que va a cifrarse es el siguiente:"
    imprime msg
    let mensajeCifrado = cifradoRSA msg clavePublica
    imprime "El mensaje se ha cifrado."
    imprime "Se va a proceder con el descifrado del mensaje..."
    let mensajeDescifrado = descifradoRSA msg clavePrivada
    let descif = transformaEnteroEnTexto (read mensajeDescifrado)
    imprime "El mensaje se ha descifrado."
    imprime ("El mensaje descifrado es: " ++ descif)
    imprime "Fin del algoritmo RSA."

algoritmoRSATradicional :: IO ()
algoritmoRSATradicional = do
    let p = 1061
    let q = 2843
    let tuplaPrimos = introduceEnTupla p q
    let n = calculoN tuplaPrimos
    let phiN = calculoPhi p q
    let clavesPublicaYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePublica = parPublico clavesPublicaYPriv                --obtiene la clave pública
    let clavePrivada = parPrivado clavesPublicaYPriv                --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePublica
    imprime "Introduce el mensaje que se va a cifrar:"
    msg <- leeMensaje
    imprime "El mensaje que va a cifrarse es el siguiente:"
    imprime msg
    let mensajeCifrado = cifradoRSA msg clavePublica
    imprime "Se va a proceder con el descifrado del mensaje..."
    let mensajeDescifrado = descifradoRSA msg clavePrivada
    let descif = transformaEnteroEnTexto (read mensajeDescifrado)
    imprime "El mensaje se ha descifrado."
    imprime ("El mensaje descifrado es: " ++ descif)
    imprime "Fin del algoritmo RSA."

cifradoBloqueACSOSimplificado :: IO()
cifradoBloqueACSOSimplificado = do
    let mensaje = "Hola, como estas? Yo estoy bien..."
    semilla <- now
    numero <- obtieneNumAleatorio
    let textoABin = transformaTextoEnBinario mensaje
    let clavePrivada = take (length textoABin) $ concat $ cambioABase2Lista $ digitos numero
    --let textoABinario = agregaCerosAIzquierda textoABin 8
    let textoABinario = separaOctetos $ codificaEnBinario mensaje
    putStr "Se va a cifrar el mensaje: "
    imprime $ show textoABinario
    imprime "Comienzo del proceso de cifrado y descifrado..."
    semilla' <- now
    --let inicial = inicializaACSegundoOrden numBits (datosInicialesAleatorios semilla') (primero textoABinario)
    let cifrado = versionSimplificadaCifrado clavePrivada (primero textoABinario) reglaAC 1 numPasos
    let binario8Bits = parte 8 (primero cifrado)
    let textoCifrado = transformaBinarioEnTexto binario8Bits
    imprime "Se va a proceder con el descifrado: "
    let binarioDescifrado = ultimo $ versionSimplificadaDescifrado clavePrivada cifrado reglaAC 1 numPasos
    let binarioDescifrado8Bits = parte 8 binarioDescifrado
    let textoDescifrado = transformaBinarioEnTexto binarioDescifrado8Bits
    putStr "El texto descifrado es: "
    imprime textoDescifrado
    imprime "¿El texto que se ha descifrado corresponde con el original?"
    if mensaje==textoDescifrado then do
        imprime "Sí."
    else do
        imprime "No."
    imprime "Fin del cifrado de bloques basado en autámatas celulares de segundo orden."

cifradoBloqueACSO :: IO ()
cifradoBloqueACSO = do
    putStrLn "  Introduzca el texto a cifrar:"
    texto <- getLine
    let textoCodificado = preparaTexto texto
    let procesoCifrado = cifrado textoCodificado
    let textoCifrado = fst'' procesoCifrado
    let datosResiduales = snd'' procesoCifrado
    let datosCASFinalesCifrado = trd'' procesoCifrado
    let clave = frt'' procesoCifrado
    let textoDescifrado = descifrado clave datosCASFinalesCifrado textoCifrado datosResiduales
    let textoDescifradoDescodificado = cambiaListasEnterosATexto (fst' textoDescifrado)
    let controlTexto = textoCodificado == fst' textoDescifrado
    imprime ("  El texto es: " ++ texto)
    -- imprime ("  El texto codificado es: " ++ show (map deListaBinarioANum textoCodificado))
    -- imprime ("  La clave es: " ++ show (deListaBinarioANum (k clave)))
    -- imprime ("  Datos iniciales 'aleatorios': " ++ show (deListaBinarioANum (kCAC clave)))
    imprime ("  Texto cifrado: " ++ show (map deListaBinarioANum textoCifrado))
    -- imprime ("  Datos residuales finales: " ++ show (deListaBinarioANum datosResiduales))
    imprime ("  Texto descifrado sin codificar: " ++ show (map deListaBinarioANum (fst' textoDescifrado)))
    imprime ("  Texto descifrado: " ++ show textoDescifradoDescodificado)
    putStr "    ¿El texto se ha descifrado correctamente? "
    if controlTexto then do
        imprime "Sí."
    else do
        imprime "No."
    imprime "Fin de la ejecución"

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
    let listaAleatoriosBase2 = concat (cambioABase2Lista listaAleatorios)
    let inicia = iniciaAC numCeldas listaAleatoriosBase2
    let automata = generaAC numCeldas (regla 30) inicia
    let indices = [1..genericLength automata-1]
    let listaCentros = concat [c | (c,i)<-zip automata indices, i==div (genericLength automata) 2]
    let num = deListaBinarioANum listaCentros
    putStr "Número aleatorio seleccionado: "
    imprime $ show num
    return num

-- Obtiene un número aleatorio de un AC de segundo orden
obtieneNumAleatorio' :: IO Int
obtieneNumAleatorio' = do
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas
    let numCeldas | even numCeldasAlt = numCeldasAlt +1
                  | otherwise = numCeldasAlt
    let listaAleatorios1 = generaAleatoriosL semillaLista
    let listaAleatorios2 = generaAleatoriosL semillaLista
    let listaAleatoriosBase2 = concat (cambioABase2Lista listaAleatorios1)
    let listaAleatoriosBase2' = concat (cambioABase2Lista listaAleatorios2)
    let inicia = inicializacion numCeldas listaAleatoriosBase2 listaAleatoriosBase2'
    let automata = generaACSO 30 1 numPasos inicia
    let indices = [1..genericLength automata-1]
    let listaCentros = concat [c | (c,i)<-zip automata indices, i==div (genericLength automata) 2]
    let num = deListaBinarioANum listaCentros
    putStr "Número aleatorio seleccionado: "
    imprime $ show num
    return num


    {----------------------------------------------------------------------
                        Pruebas AC de segundo orden
    ----------------------------------------------------------------------}

pruebaAutomataSOAleatorio :: IO ()
pruebaAutomataSOAleatorio = do
    putStr "Se va a generar un autómata en "
    putStr $ show numPasos
    imprime " pasos."
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas
    let numCeldas | even numCeldasAlt = numCeldasAlt +1
                  | otherwise = numCeldasAlt
    let listaAleatorios1 = generaAleatoriosL semillaLista
    let listaAleatorios2 = generaAleatoriosL semillaLista
    let listaAleatoriosBase2 = concat (cambioABase2Lista listaAleatorios1)
    let listaAleatoriosBase2' = concat (cambioABase2Lista listaAleatorios2)
    let inicia = inicializacion numCeldas listaAleatoriosBase2 listaAleatoriosBase2'
    putStr "La configuración inicial es: "
    imprime $ show inicia
    let listaReglaAplicada = aplicaReglaSegundoOrden reglaAC 3 (pasado inicia) (presente inicia)
    let automata = generaACSO reglaAC 3 numPasos inicia
    imprime "Así se vería el autómata: "
    muestraACSO numPasos reglaAC 3 numCeldas inicia