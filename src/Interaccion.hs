module Interaccion where

import AutomataCelular1D
import RSA
import UtilGeneral
import UtilCripto
import UtilIO
import Tipos
import Constantes
import AutomataCelularSO
import CifradoBloque
---
import Data.List as L
import System.Random
import System.IO

mainInteraccion :: IO ()
mainInteraccion = do
    imprime "Cargando..."
    principal
    --pruebaAutomataSO
    --pruebaAutomataSOAleatorio
    --obtieneNumAleatorio'
    --compruebaTamanoBitsTexto
    --compruebaFuncionamientoTransformacionesTexto
    --mainDepuracion
    --mainDepuracion2
    imprime "Fin de la ejecución."

principal :: IO ()
principal = do
    imprime "Advertencia: el programa puede tardar unos segundos en generar los primos."
    algoritmoRSA
    -- imprime "¿Qué algoritmo desea aplicar: RSA, Cifrado de bloques (bloque) o XXX? "
    -- opcion <- leeMensaje
    -- if opcion == "RSA" || opcion=="rsa" then do
    --     imprime "Se está ejecutando el algoritmo RSA..."
    --     algoritmoRSA
    -- else if opcion == "Cifrado de bloques" || opcion=="bloque" then do
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
    -- imprime "El tamaño del autómata es: "
    -- imprime $ show $ L.genericLength automata
    -- imprime "La mitad es: "
    -- imprime $ show (div (L.genericLength automata) 2)
    let indices = [1..L.genericLength automata-1]
    let listaCentros = L.concat [c | (c,i)<-zip automata indices, i==div (L.genericLength automata) 2]
    -- imprime "La lista de centros es: "
    -- imprime $ show listaCentros
    let num = deListaBinarioANum listaCentros
    -- imprime "Número obtenido:"
    -- imprime $ show num
    let control = esPrimo (toInteger num)
    semillaPrimos <- now
    let opcion = generaAleatorio' semillaPrimos             --obtiene un número aleatorio entre 0 y 1 para elegir de manera aleatoria si se buscará un primo por encima o por debajo del número
    --se genera el número primo a partir del AC
    let p | control = num                                   --si el número ya es primo, no hay que buscarlo
          | opcion == 0 = obtienePrimoCercanoInf num
          | otherwise = obtienePrimoCercanoSup num
    let mensaje = "El número primo generado es: " ++ show p
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
    let phiN = calculoPhi (toInteger p) (toInteger q)
    imprime "Se van a generar la clave pública y la clave privada de RSA. Esta operación puede tardar."
    let clavesPubYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePub = parPublico clavesPubYPriv               --obtiene la clave pública
    let clavePriv = parPrivado clavesPubYPriv              --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePub
    putStr "La clave privada es: "
    imprime $ show clavePriv
    imprime "Introduce el mensaje que se va a cifrar:"
    msg <- leeMensaje
    --let msg = "Lorem ipsum dolor sit amet consectetur adipiscing elit ornare mattis et, ad parturient eu nec eleifend mollis tincidunt facilisi ligula tortor nostra, quam torquent sollicitudin rutrum interdum dignissim duis nulla posuere. Accumsan etiam donec leo lacus sollicitudin nostra nec eu, enim natoque inceptos dui mus proin sapien ridiculus cursus, potenti senectus ac molestie facilisis iaculis phasellus. Integer ac tristique inceptos at malesuada fermentum blandit nullam, id consequat urna potenti praesent senectus dis cum, dictum libero lobortis vehicula tincidunt habitasse augue. Faucibus venenatis nisl sapien rutrum morbi porta ac metus a, eget nullam viverra suspendisse mus penatibus suscipit egestas nostra ridiculus, in consequat porttitor turpis sodales orci massa sociis. Aliquam varius hendrerit pretium posuere cum consequat felis habitasse tristique, nisl aenean auctor est curae pharetra tellus nec litora, ullamcorper vivamus nulla suspendisse class laoreet cubilia orci. Nostra rhoncus vestibulum sociis bibendum luctus accumsan pretium, maecenas pulvinar magnis volutpat potenti sollicitudin sodales turpis, lobortis leo convallis proin phasellus cum."
    imprime "El mensaje que va a cifrarse es el siguiente:"
    imprime msg
    --
    -- let mensajeCifrado = encriptaRSA msg clavePub
    -- imprime "El mensaje se ha cifrado correctamente de la siguiente manera:"
    -- --imprime $ show (fst mensajeCifrado)
    -- imprime $ show mensajeCifrado
    -- imprime "Se va a proceder con el descifrado del mensaje..."
    -- let mensajeOriginal  = desencriptaRSA mensajeCifrado clavePriv
    -- imprime "El mensaje se ha descifrado y es el siguiente: "
    -- imprime $ show mensajeOriginal
    --
    let mensajeCifrado = cifraMensaje msg clavePub
    imprime "El mensaje se ha cifrado correctamente de la siguiente manera:"
    --imprime $ show (fst mensajeCifrado)
    imprime $ show mensajeCifrado
    imprime "Se va a proceder con el descifrado del mensaje..."
    let mensajeOriginal  = descifraMensaje mensajeCifrado clavePriv
    imprime "El mensaje se ha descifrado y es el siguiente: "
    imprime $ show mensajeOriginal



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

-- Obtiene un número aleatorio de un AC de segundo orden
obtieneNumAleatorio' :: IO Int
obtieneNumAleatorio' = do
    semillaLista <- now
    semillaCeldas <- now
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas
    let numCeldas | even numCeldasAlt = numCeldasAlt +1
                  | otherwise = numCeldasAlt
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)
    let inicia = inicializa numCeldas 30 lAleatBase2
    let automata = generaACSO 30 numPasos inicia numCeldas
    let indices = [1..L.genericLength automata-1]
    let listaCentros = L.concat [c | (c,i)<-zip automata indices, i==div (L.genericLength automata) 2]
    let num = deListaBinarioANum listaCentros
    putStr "Número aleatorio seleccionado: "
    imprime $ show num
    return num


    {----------------------------------------------------------------------
                        Pruebas AC de segundo orden
    ----------------------------------------------------------------------}

pruebaAutomataSO :: IO ()
pruebaAutomataSO = do
    putStr "Se va a generar un autómata en "
    putStr $ show numPasos
    imprime " pasos."
    let n = div minCeldas 2
    let lista = L.replicate n 0 L.++ [1] L.++ L.replicate n 0
    let inicia = inicializa minCeldas reglaAC lista
    muestraACSO' numPasos reglaAC 2 minCeldas inicia

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
    let listaAleatorios = generaAleatoriosL semillaLista
    let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)
    let inicia = inicializa numCeldas reglaAC lAleatBase2
    putStr "La configuración inicial es: "
    imprime $ show inicia
    let listaReglaAplicada = aplicaReglaSO' reglaAC 3 (extraePasado inicia) (extraePresente inicia)
    let automata = generaACSO' reglaAC 3 numPasos inicia
    imprime "Así se vería el autómata: "
    muestraACSO' numPasos reglaAC 3 numCeldas inicia

compruebaTamanoBitsTexto :: IO ()
compruebaTamanoBitsTexto = do
    imprime "Introduce el texto: "
    msg <- leeMensaje
    let tam = compruebaTamBits msg
    putStr "El tamaño del texto es: "
    imprime $ show tam

compruebaFuncionamientoTransformacionesTexto :: IO ()
compruebaFuncionamientoTransformacionesTexto = do
    imprime "Ahora vamos a ver cómo se traduce el texto..."
    let mensajeOriginal = "Hola, mundo"
    let bits = traduceTextoABinario' mensajeOriginal
    let mensajeRecuperado = binarioATexto bits
    putStrLn $ "Mensaje original: " ++ mensajeOriginal
    putStrLn $ "Texto transformado: " ++ show bits
    putStrLn $ "Mensaje recuperado: " ++ mensajeRecuperado
    imprime "A continuación vamos a probar si funciona bien al meter el texto por teclado."
    imprime "Introduce el texto: "
    msg <- leeMensaje
    let bits' = traduceTextoABinario' msg
    let mensajeRecuperado' = binarioATexto bits'
    putStrLn $ "Mensaje original: " ++ msg
    putStrLn $ "Texto transformado: " ++ show bits'
    putStrLn $ "Mensaje recuperado: " ++ mensajeRecuperado'


mainDepuracion :: IO ()
mainDepuracion = do
    p <- obtienePrimoAleatorio
    q <- obtienePrimoAleatorio
    let tuplaPrimos = introduceEnTupla (toInteger p) (toInteger q)
    putStr "Los primos p y q son los siguientes: "
    imprime $ show tuplaPrimos
    let n = calculoN tuplaPrimos
    let phiN = calculoPhi (toInteger p) (toInteger q)
    let clavesPubYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePub = parPublico clavesPubYPriv               --obtiene la clave pública
    let clavePriv = parPrivado clavesPubYPriv              --obtiene la clave privada
    -- imprime "Introduce el mensaje a cifrar: "
    -- mensajeOriginal <- leeMensaje
    let mensajeOriginal = "Hola, mundo"
    putStr "El mensaje que se va a encriptar es: "
    imprime mensajeOriginal
    let mensajeEncriptado = encriptaRSA mensajeOriginal clavePub
    let mensajeDesencriptado = desencriptaRSA mensajeEncriptado clavePub
    putStrLn $ "Mensaje Original: " ++ mensajeOriginal
    putStrLn $ "Representación Original: " ++ mostrarRepresentacion mensajeOriginal
    putStrLn $ "Representación Numérica: " ++ mostrarRepresentacionNumerica mensajeOriginal
    putStrLn $ "Mensaje Encriptado: " ++ mensajeEncriptado
    putStrLn $ "Mensaje Desencriptado: " ++ mensajeDesencriptado
    putStrLn $ "Representación Desencriptada: " ++ mostrarRepresentacion mensajeDesencriptado
    putStrLn $ "Representación Numérica Desencriptada: " ++ mostrarRepresentacionNumerica mensajeDesencriptado


mainDepuracion2 :: IO ()
mainDepuracion2 = do
    p <- obtienePrimoAleatorio
    q <- obtienePrimoAleatorio
    let tuplaPrimos = introduceEnTupla (toInteger p) (toInteger q)
    let n = calculoN tuplaPrimos
    let phiN = calculoPhi (toInteger p) (toInteger q)
    imprime "Se van a generar la clave pública y la clave privada de RSA. Esta operación puede tardar."
    let clavesPubYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePub = parPublico clavesPubYPriv               --obtiene la clave pública
    let clavePriv = parPrivado clavesPubYPriv
    imprime "Se ha configurado las claves privada y pública."
    imprime "Introduce un mensaje: "
    mensajeOriginal <- leeMensaje
    putStr "El mensaje que se va a encriptar es: "
    imprime mensajeOriginal
    let mensajeEncriptado = cifraMensaje mensajeOriginal clavePub
    imprime "El mensaje se ha cifrado:"
    imprime mensajeEncriptado
    let mensajeDesencriptado = descifraMensaje mensajeEncriptado clavePriv
    imprime "El mensaje se ha descifrado:"
    imprime mensajeDesencriptado