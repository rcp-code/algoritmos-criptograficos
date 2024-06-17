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
        mainWolfram
    else do
        imprime "No se ha seleccionado una opción válida."
    imprime "Fin de la ejecución de principal."

algoritmoRSA :: IO ()
algoritmoRSA = do
    putStrLn "Introduzca el texto a cifrar:"
    texto <- getLine
    p <- AutomataCelular.obtienePrimoAleatorio
    q <- AutomataCelular.obtienePrimoAleatorio
    imprime "Los primos se han generado correctamente."
    let phiN = calculoPhi p q
    let n   = calculoN (p, q)
    let claves = clavesPublicaYPrivada (p, q)
    let privada = parPrivado claves
    let publica = parPublico claves
    let e = snd publica
    let d = snd privada
    let cif = cifradoRSA texto (n,e)
    let descif = descifradoRSA cif (n,d)
    let menDescif = transformaEnteroEnTexto (read descif)
    imprime ("Clave pública: " ++ show publica)
    imprime "El texto ha sido cifrado."
    putStr "El texto se ha descifrado "
    if menDescif == texto then do
        imprime "correctamente."
        imprime ("Texto descifrado: " ++ show menDescif)
    else do
        imprime "de forma incorrecta."
        imprime ("Texto descifrado: " ++ show menDescif)

algoritmoRSATradicional :: IO ()
algoritmoRSATradicional = do
    let p = 1061
    let q = 2843
    putStrLn "Introduzca el texto a cifrar:"
    texto <- getLine
    let phiN = calculoPhi p q
    let n   = calculoN (p, q)
    let claves = clavesPublicaYPrivada (p, q)
    let privada = parPrivado claves
    let publica = parPublico claves
    let e = snd publica
    let d = snd privada
    let cif = cifradoRSA texto (n,e)
    let descif = descifradoRSA cif (n,d)
    let menDescif = transformaEnteroEnTexto (read descif)
    imprime ("Clave pública: " ++ show publica)
    imprime "El texto ha sido cifrado."
    putStr "El texto se ha descifrado "
    if menDescif == texto then do
        imprime "correctamente."
        imprime ("Texto descifrado: " ++ show menDescif)
    else do
        imprime "de forma incorrecta."
        imprime ("Texto descifrado: " ++ show menDescif)

cifradoBloqueACSOSimplificado :: IO()
cifradoBloqueACSOSimplificado = do
    semilla <- now
    imprime "Introduce el texto que quieres cifrar: "
    texto <- getLine
    imprime "Elige el radio de vecindad del autómata celular para el cifrado/descifrado: "
    radio <- getLine
    imprime "Introduce tu clave de cifrado: "
    claveSecreta <- getLine
    let textoPreparado = concat $ preparaMensaje texto
    let tamTexto = length textoPreparado
    let radioVecindad = read radio :: Int
    let claveCodificada = codificaEnBinario claveSecreta
    let clave = if length claveCodificada == tamTexto then do claveCodificada else do agregaCerosAIzquierda claveCodificada tamTexto
    let procesoCifrado = versionSimplificadaCifrado clave textoPreparado reglaAC radioVecindad numPasos
    let textoCifrado = primero procesoCifrado
    let residuales = ultimo procesoCifrado
    let procesoDescifrado = versionSimplificadaDescifrado clave procesoCifrado reglaAC radioVecindad numPasos
    let textoDescifrado = primero procesoDescifrado
    imprime ("  El mensaje codificado es: " ++ show (deListaBinarioANum textoPreparado))
    imprime ("  El mensaje cifrado es: " ++ show (deListaBinarioANum textoCifrado))
    imprime ("  El mensaje descifrado es: " ++ show (deListaBinarioANum textoDescifrado))
    let iguales = textoPreparado == textoDescifrado
    if iguales then do
        imprime "   El texto se ha descifrado correctamente."
        imprime ("  Mensaje original: " ++ texto)
        imprime ("  Mensaje descifrado: " ++ show (cambiaATexto textoDescifrado))
    else do
        imprime "   El mensaje NO se ha descifrado bien."
    imprime "Fin del cifrado de bloques basado en autámatas celulares de segundo orden."

cifradoBloqueACSO :: IO ()
cifradoBloqueACSO = do
    putStrLn "  Introduzca el texto a cifrar:"
    texto <- getLine
    let procesoCifrado = cifrado texto
    let textoCifrado = fst'' procesoCifrado
    let datosResiduales = snd'' procesoCifrado
    let datosCASFinalesCifrado = trd'' procesoCifrado
    let clave = frt'' procesoCifrado
    let procesoDescifrado = descifrado clave datosCASFinalesCifrado textoCifrado datosResiduales
    let textoDescifrado = fst' procesoDescifrado
    let controlTexto = texto == textoDescifrado
    imprime ("  El mensaje es: " ++ texto)
    imprime ("  La clave es: " ++ show (deListaBinarioANum (k clave)))
    imprime ("  Datos iniciales 'aleatorios': " ++ show (deListaBinarioANum (kCAC clave)))
    imprime ("  Mensaje cifrado: " ++ show (map deListaBinarioANum textoCifrado))
    imprime ("  Datos residuales finales: " ++ show (deListaBinarioANum datosResiduales))
    imprime ("  Mensaje descifrado: " ++ textoDescifrado)
    putStr "    ¿El mensaje se ha descifrado correctamente? "
    if controlTexto then do
        imprime "Sí."
    else do
        imprime "No."
    imprime "Fin de la ejecución"

-- Obtiene un número aleatorio como semilla:
obtieneNumAleatorio :: IO Int
obtieneNumAleatorio = do
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