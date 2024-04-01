module Interaccion where

import AutomataCelular
import RSA
import UtilGeneral
import UtilCripto
import UtilIO
import Tipos
import Constantes
import CifradoBloque
---
import Data.List
import System.Random
import System.IO

mainInteraccion :: IO ()
mainInteraccion = do
    imprime "Cargando..."
    principal
    imprime "Fin de la ejecución."

principal :: IO ()
principal = do
    imprime "Advertencia: el programa puede tardar unos segundos en generar los primos y en realizar los procesos de cifrado y descifrado mediante autómatas celulares."
    --algoritmoRSA
    --cifradoBloqueACSOSimplificado
    --cifradoBloqueACSO
    cifradoBloqueACSO2
    -- imprime "¿Qué algoritmo desea aplicar: RSA, Cifrado de bloques (bloque) o XXX? "
    -- opcion <- leeMensaje
    -- if opcion == "RSA" || opcion=="rsa" then do
    --     imprime "Se está ejecutando RSA..."
    --     algoritmoRSA
    -- else if opcion == "Cifrado de bloques" || opcion=="bloque" then do
    --     imprime "Se está ejecutando Cifrado de bloques basado en AC de segundo orden..."
    --     cifradoBloqueACSO
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
    let listaAleatoriosBase2 = concat (cambioABase2Lista listaAleatorios)                --se pasa la lista de aleatorios a base 2 y se aplana
    let inicia = iniciaAC numCeldas listaAleatoriosBase2
    let automata = generaAC numCeldas (regla 30) inicia
    -- imprime "El tamaño del autómata es: "
    -- imprime $ show $ L.genericLength automata
    -- imprime "La mitad es: "
    -- imprime $ show (div (L.genericLength automata) 2)
    let indices = [1..genericLength automata-1]
    let listaCentros = concat [c | (c,i)<-zip automata indices, i==div (genericLength automata) 2]
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
    let clavesPublicaYPriv = clavesPublicaYPrivada tuplaPrimos
    let clavePublica = parPublico clavesPublicaYPriv               --obtiene la clave pública
    let clavePrivada = parPrivado clavesPublicaYPriv              --obtiene la clave privada
    putStr "La clave pública es: "
    imprime $ show clavePublica
    putStr "La clave privada es: "
    imprime $ show clavePrivada
    imprime "Introduce el mensaje que se va a cifrar:"
    msg <- leeMensaje
    imprime "El mensaje que va a cifrarse es el siguiente:"
    imprime msg
    let mensajeCifrado = cifraRSA msg clavePublica
    imprime "El mensaje se ha cifrado correctamente de la siguiente manera:"
    imprime mensajeCifrado
    imprime "Se va a proceder con el descifrado del mensaje..."
    let mensajeOriginal = descifraRSA mensajeCifrado clavePrivada
    imprime "El mensaje se ha descifrado y es el siguiente:"
    imprime $ show mensajeOriginal
    imprime "Fin del algoritmo RSA."

cifradoBloqueACSOSimplificado :: IO()
cifradoBloqueACSOSimplificado = do
    imprime "Introduce el mensaje que quieras cifrar: "
    mensaje <- leeMensaje
    semilla <- now
    numero <- obtieneNumAleatorio
    let clavePrivada = deListaBinarioANum $ take 8 $ concat $ cambioABase2Lista $ digitos numero
    let textoABin = transformaTextoEnBinario mensaje
    let textoABinario = agregaCerosAIzquierda textoABin 8
    putStr "Se va a cifrar el mensaje: "
    imprime mensaje
    imprime "Comienzo del proceso de cifrado y descifrado..."
    semilla' <- now
    let inicial = inicializaACSegundoOrden numBits (datosInicialesAleatorios semilla') textoABinario
    let cifrado = versionSimplificadaCifrado clavePrivada inicial reglaAC 1 numPasos
    let binario8Bits = parte 8 (primero cifrado)
    let textoCifrado = transformaBinarioEnTexto binario8Bits
    imprime "Se va a proceder con el descifrado: "
    let binarioDescifrado = ultimo $ versionSimplificadaDescifrado clavePrivada cifrado reglaAC 2 numPasos
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
    imprime "Introduce el mensaje que quieras cifrar: "
    mensaje <- leeMensaje
    semilla <- now
    let clavePrivada = take 224 $ concat $ cambioABase2Lista $ generaAleatoriosL semilla
    let textoABinario = bloques64Bits $ transformaTextoEnBinario mensaje
    putStr "Se va a cifrar el mensaje: "
    imprime mensaje
    imprime "Comienzo del proceso de cifrado y descifrado..."
    if length (ultimo textoABinario) < numBits then do
        let subLista = agregaCerosAIzquierda (ultimo textoABinario) numBits
        let textoABinario' = init textoABinario ++ [subLista]
        semilla' <- now
        let tripletaCifradoCASAleatoriosFinal = cifrado semilla' numeroRondas clavePrivada textoABinario reglaAC
        let textoCifrado = transformaBinarioEnTexto $ fst' tripletaCifradoCASAleatoriosFinal
        putStr "El texto cifrado es: "
        imprime textoCifrado
        imprime "Se va a proceder con el descifrado: "
        let binarioDescifrado = descifrado numeroRondas clavePrivada (snd' tripletaCifradoCASAleatoriosFinal) (fst' tripletaCifradoCASAleatoriosFinal) (trd' tripletaCifradoCASAleatoriosFinal) reglaAC
        let textoDescifrado = transformaBinarioEnTexto binarioDescifrado
        putStr "El texto descifrado es: "
        imprime textoDescifrado
        imprime "¿El texto que se ha descifrado corresponde con el original?"
        if mensaje==textoDescifrado then do
            imprime "Sí."
        else do
            imprime "No."
        imprime "Fin de la ejecución"
    else do
        semilla' <- now
        let tripletaCifradoCASAleatoriosFinal = cifrado semilla' numeroRondas clavePrivada textoABinario reglaAC
        let textoCifrado = transformaBinarioEnTexto $ fst' tripletaCifradoCASAleatoriosFinal
        putStr "El texto cifrado es: "
        imprime textoCifrado
        imprime "Se va a proceder con el descifrado: "
        let binarioDescifrado = descifrado numeroRondas clavePrivada (snd' tripletaCifradoCASAleatoriosFinal) (fst' tripletaCifradoCASAleatoriosFinal) (trd' tripletaCifradoCASAleatoriosFinal) reglaAC
        let textoDescifrado = transformaBinarioEnTexto binarioDescifrado
        putStr "El texto descifrado es: "
        imprime textoDescifrado
        imprime "¿El texto que se ha descifrado corresponde con el original?"
        if mensaje==textoDescifrado then do
            imprime "Sí."
        else do
            imprime "No."
    imprime "Fin de la ejecución"

cifradoBloqueACSO2 :: IO ()
cifradoBloqueACSO2 = do
    let mensaje = "Lorem ipsum dolor sit amet consectetur adipiscing elit, facilisis curabitur habitant pellentesque sollicitudin nostra nibh, mi lacus semper tempor aliquet potenti. Vivamus nam vestibulum aenean dis suspendisse eget augue quam, tempor pellentesque praesent gravida sapien turpis. Nascetur dapibus sapien himenaeos rutrum arcu morbi, suspendisse nulla tortor vestibulum conubia egestas, viverra aptent mi sociosqu nullam. Et felis mus sed nam per vivamus sem congue penatibus iaculis varius, vulputate ultrices sociosqu duis tempus dictumst sodales condimentum nulla a, placerat habitant eleifend nunc primis convallis velit tincidunt litora ligula. Scelerisque platea maecenas cras laoreet lobortis tortor proin aliquet suscipit ridiculus cubilia commodo augue, eros rhoncus sagittis diam eget phasellus nam non eu justo netus tempor. Tempor platea justo habitasse ornare varius malesuada facilisi aenean odio, aptent sollicitudin cras a blandit faucibus euismod fermentum mi, eleifend non facilisis vulputate fames donec ac nulla."
    semilla <- now
    let clavePrivada = take 224 $ concat $ cambioABase2Lista $ generaAleatoriosL semilla
    let textoABinario = bloques64Bits $ transformaTextoEnBinario mensaje
    putStr "Se va a cifrar el mensaje: "
    imprime mensaje
    imprime "Comienzo del proceso de cifrado y descifrado..."
    if length (ultimo textoABinario) < numBits then do
        let subLista = agregaCerosAIzquierda (ultimo textoABinario) numBits
        let textoABinario' = init textoABinario ++ [subLista]
        semilla' <- now
        let tripletaCifradoCASAleatoriosFinal = cifrado semilla' numeroRondas clavePrivada textoABinario reglaAC
        let textoCifradoBin = fst' tripletaCifradoCASAleatoriosFinal
        
        --let partebs = map (deListaBinarioANum . parte 8) textoCifradoBin
        --let listaEnteros = [deListaBinarioANum bin | bin<-partebs]
        --imprime ("La lista de enteros es: " ++ show partebs)
        --let textoCifrado = transformaEnteroEnTexto listaEnteros
        --putStr "El texto cifrado es: "
        --imprime textoCifrado
        imprime "Se va a proceder con el descifrado: "
        let binarioDescifrado = descifrado numeroRondas clavePrivada (snd' tripletaCifradoCASAleatoriosFinal) (fst' tripletaCifradoCASAleatoriosFinal) (trd' tripletaCifradoCASAleatoriosFinal) reglaAC
        let textoDescifrado = transformaBinarioEnTexto binarioDescifrado
        putStr "El texto descifrado es: "
        imprime textoDescifrado
        imprime "¿El texto que se ha descifrado corresponde con el original?"
        if mensaje==textoDescifrado then do
            imprime "Sí."
        else do
            imprime "No."
        imprime "Fin de la ejecución"
    else do
        semilla' <- now
        let tripletaCifradoCASAleatoriosFinal = cifrado semilla' numeroRondas clavePrivada textoABinario reglaAC
        let textoCifrado = transformaBinarioEnTexto $ fst' tripletaCifradoCASAleatoriosFinal
        putStr "El texto cifrado es: "
        imprime textoCifrado
        imprime "Se va a proceder con el descifrado: "
        let binarioDescifrado = descifrado numeroRondas clavePrivada (snd' tripletaCifradoCASAleatoriosFinal) (fst' tripletaCifradoCASAleatoriosFinal) (trd' tripletaCifradoCASAleatoriosFinal) reglaAC
        let textoDescifrado = transformaBinarioEnTexto binarioDescifrado
        putStr "El texto descifrado es: "
        imprime textoDescifrado
        imprime "¿El texto que se ha descifrado corresponde con el original?"
        if mensaje==textoDescifrado then do
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
    let listaReglaAplicada = aplicaReglaSO reglaAC 3 (extraePasado inicia) (extraePresente inicia)
    let automata = generaACSO reglaAC 3 numPasos inicia
    imprime "Así se vería el autómata: "
    muestraACSO numPasos reglaAC 3 numCeldas inicia

    {----------------------------------------------------------------------
                        Otras pruebas de funcionamiento
    ----------------------------------------------------------------------}

-- compruebaTamanoBitsTexto :: IO ()
-- compruebaTamanoBitsTexto = do
--     imprime "Introduce el texto: "
--     msg <- leeMensaje
--     let tam = compruebaTamBits msg
--     putStr "El tamaño del texto es: "
--     imprime $ show tam

-- compruebaFuncionamientoTransformacionesTexto :: IO ()
-- compruebaFuncionamientoTransformacionesTexto = do
--     imprime "Ahora vamos a ver cómo se traduce el texto..."
--     let mensajeOriginal = "Hola, mundo"
--     let bits = traduceTextoABinario' mensajeOriginal
--     let mensajeRecuperado = binarioATexto bits
--     putStrLn $ "Mensaje original: " ++ mensajeOriginal
--     putStrLn $ "Texto transformado: " ++ show bits
--     putStrLn $ "Mensaje recuperado: " ++ mensajeRecuperado
--     imprime "A continuación vamos a probar si funciona bien al meter el texto por teclado."
--     imprime "Introduce el texto: "
--     msg <- leeMensaje
--     let bits' = traduceTextoABinario' msg
--     let mensajeRecuperado' = binarioATexto bits'
--     putStrLn $ "Mensaje original: " ++ msg
--     putStrLn $ "Texto transformado: " ++ show bits'
--     putStrLn $ "Mensaje recuperado: " ++ mensajeRecuperado'

-- mainDepuracion :: IO ()
-- mainDepuracion = do
--     p <- obtienePrimoAleatorio
--     q <- obtienePrimoAleatorio
--     let tuplaPrimos = introduceEnTupla (toInteger p) (toInteger q)
--     putStr "Los primos p y q son los siguientes: "
--     imprime $ show tuplaPrimos
--     let n = calculoN tuplaPrimos
--     let phiN = calculoPhi (toInteger p) (toInteger q)
--     let clavesPublicaYPriv = clavesPublicaYPrivada tuplaPrimos
--     let clavePublica = parPublico clavesPublicaYPriv               --obtiene la clave pública
--     let clavePrivada = parPrivado clavesPublicaYPriv              --obtiene la clave privada
--     -- imprime "Introduce el mensaje a cifrar: "
--     -- mensajeOriginal <- leeMensaje
--     let mensajeOriginal = "Hola, mundo"
--     putStr "El mensaje que se va a encriptar es: "
--     imprime mensajeOriginal
--     let mensajeCifrado = cifraRSA mensajeOriginal clavePublica
--     let mensajeDescifrado = descifraRSA mensajeCifrado clavePublica
--     putStrLn $ "Mensaje Original: " ++ mensajeOriginal
--     putStrLn $ "Representación Original: " ++ mostrarRepresentacion mensajeOriginal
--     putStrLn $ "Representación Numérica: " ++ mostrarRepresentacionNumerica mensajeOriginal
--     putStrLn $ "Mensaje Encriptado: " ++ mensajeCifrado
--     putStrLn $ "Mensaje Desencriptado: " ++ mensajeDescifrado
--     putStrLn $ "Representación Desencriptada: " ++ mostrarRepresentacion mensajeDescifrado
--     putStrLn $ "Representación Numérica Desencriptada: " ++ mostrarRepresentacionNumerica mensajeDescifrado

-- mainDepuracion2 :: IO ()
-- mainDepuracion2 = do
--     p <- obtienePrimoAleatorio
--     q <- obtienePrimoAleatorio
--     let tuplaPrimos = introduceEnTupla (toInteger p) (toInteger q)
--     let n = calculoN tuplaPrimos
--     let phiN = calculoPhi (toInteger p) (toInteger q)
--     imprime "Se van a generar la clave pública y la clave privada de RSA. Esta operación puede tardar."
--     let clavesPublicaYPriv = clavesPublicaYPrivada tuplaPrimos
--     let clavePublica = parPublico clavesPublicaYPriv               --obtiene la clave pública
--     let clavePrivada = parPrivado clavesPublicaYPriv
--     imprime "Se ha configurado las claves privada y pública."
--     imprime "Introduce un mensaje: "
--     mensajeOriginal <- leeMensaje
--     putStr "El mensaje que se va a encriptar es: "
--     imprime mensajeOriginal
--     let mensajeCifrado = cifraMensaje mensajeOriginal clavePublica
--     imprime "El mensaje se ha cifrado:"
--     imprime mensajeCifrado
--     let mensajeDescifrado = descifraMensaje mensajeCifrado clavePrivada
--     imprime "El mensaje se ha descifrado:"
--     imprime mensajeDescifrado