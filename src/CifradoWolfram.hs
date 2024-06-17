module CifradoWolfram where

    {----------------------------------------------------------------------
                            Cifrado de Wolfram
    ----------------------------------------------------------------------}

import Constantes
import UtilCripto
import AutomataCelular as AC
import Tipos
import UtilIO
import UtilGeneral
import Data.List
import GHC.IO (unsafePerformIO)

    {----------------------------------------------------------------------
                                Preparativos
    ----------------------------------------------------------------------}

--Codifica el mensaje y además obtiene el tamaño del bloque
preparaTextoParaCifrado :: Mensaje -> ([Int], Int)
preparaTextoParaCifrado mensaje = (mensajeCodificado, length mensajeCodificado)
    where
        mensajeCodificado = codificaEnBinario mensaje

--Configuración inicial del autómata a partir de la clave secreta (es necesario el tamaño del mensaje para que el tamaño de la clave sea el mismo)
configuracionInicialAutomata :: String -> Int -> [Int]
configuracionInicialAutomata claveSecreta tamTexto
    | length claveCodificada /= tamTexto = agregaCerosAIzquierda claveCodificada tamTexto
    | otherwise = claveCodificada
    where 
        claveCodificada = codificaEnBinario claveSecreta

    {----------------------------------------------------------------------
                        Algoritmos cifrado y descifrado
    ----------------------------------------------------------------------}

--Proceso del cifrado Wolfram
cifradoWolfram :: String -> Mensaje -> ([Int], [Int])
cifradoWolfram claveSecreta mensaje = (xorl clave msgPreparado, clave)
    where
        semilla = unsafePerformIO obtieneNumeroAleatorioMedianteAC
        (msgPreparado, tamTexto) = preparaTextoParaCifrado mensaje
        configuracionInicial = configuracionInicialAutomata claveSecreta tamTexto
        inicia = iniciaAC maxCeldas configuracionInicial
        automata = generaAC evolucionCifradoWolfram (regla 30) inicia
        tamAutomata = length automata
        numAleatorio = generaAleatorio semilla 0 tamAutomata
        numAleatorioComienzo = tamAutomata - length msgPreparado
        filasAutomata = [c | (c,i)<-zip automata [0..genericLength automata-1], i `elem` [numAleatorioComienzo..genericLength automata-1]]
        obtieneCeldaCentral a = head [x | (x,i) <- zip a [0..(length a - 1)], i == div (length a) 2]
        clave = [obtieneCeldaCentral x | x<-filasAutomata]

--Proceso del descifrado Wolfram
descifradoWolfram :: [Int] -> [Int] -> Mensaje
descifradoWolfram mensajeCifrado clave = descodifica descifradoXOR
    where
        descifradoXOR = xorl clave mensajeCifrado

    {----------------------------------------------------------------------
                            Unión de los procesos
    ----------------------------------------------------------------------}

mainWolfram :: IO()
mainWolfram = do
    putStrLn "Introduzca el texto a cifrar:"
    mensaje <- getLine
    putStrLn "Introduzca una clave secreta para el cifrado:"
    claveSecreta <- getLine
    let procesoCifrado = cifradoWolfram claveSecreta mensaje
    let mensajeDescifrado = uncurry descifradoWolfram procesoCifrado
    let control = mensaje == mensajeDescifrado
    if control then do
        imprime "El mensaje se ha cifrado y descifrado correctamente."
    else do
        imprime "Algo ha fallado en el proceso de cifrado/descifrado de Wolfram."
    imprime ("Texto original: " ++ show mensaje)
    imprime ("Texto descifrado: " ++ show mensajeDescifrado)

