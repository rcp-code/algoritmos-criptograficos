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



    {----------------------------------------------------------------------
                                Preparativos
    ----------------------------------------------------------------------}

preparaTextoParaElCifrado :: Mensaje -> ([Int], Int)
preparaTextoParaElCifrado mensaje = (mensajeCodificado, length mensajeCodificado)
    where
        mensajeCodificado = codificaEnBinario mensaje

codificaClave :: String -> [Int]
codificaClave = codificaEnBinario

configuracionInicialAutomata :: [Int] -> Int -> [Int]
configuracionInicialAutomata claveCodificada tamTexto
    | length claveCodificada /= tamTexto = agregaCerosAIzquierda claveCodificada tamTexto
    | otherwise = claveCodificada

    {----------------------------------------------------------------------
                        Algoritmos cifrado y descifrado
    ----------------------------------------------------------------------}

cifradoWolfram :: [Int] -> Int -> [Int] -> ([Int], [Int])
cifradoWolfram configuracionInicial semilla mensajeCodificado = (xorl clave mensajeCodificado, clave)
    where
        inicia = iniciaAC maxCeldas configuracionInicial
        automata = generaAC evolucionCifradoWolfram (regla 30) inicia
        tamAutomata = length automata
        numAleatorio = generaAleatorio semilla 0 tamAutomata
        numAleatorioComienzo = tamAutomata - length mensajeCodificado
        filasAutomata = [c | (c,i)<-zip automata [0..genericLength automata-1], i `elem` [numAleatorioComienzo..genericLength automata-1]]
        obtieneCeldaCentral a = head [x | (x,i) <- zip a [0..(length a - 1)], i == div (length a) 2]
        clave = [obtieneCeldaCentral x | x<-filasAutomata]

descifradoWolfram :: [Int] -> [Int] -> Mensaje
descifradoWolfram mensajeCifrado clave = descodifica descifradoXOR
    where
        descifradoXOR = xorl clave mensajeCifrado

    {----------------------------------------------------------------------
                            Unión de los procesos
    ----------------------------------------------------------------------}

cifradoWolframIO :: IO Mensaje
cifradoWolframIO = do
    semilla <- now
    putStrLn "Introduzca el texto a cifrar:"
    texto <- getLine
    let textoTransformado = codificaEnBinario texto
    let tamTextoTransformado = length textoTransformado
    let claveSecreta = "MIRANDA"
    let claveCodificada = codificaEnBinario claveSecreta
    let confInicial | length claveCodificada /= tamTextoTransformado = agregaCerosAIzquierda claveCodificada tamTextoTransformado
                    | otherwise = claveCodificada
    let inicia = iniciaAC maxCeldas confInicial
    let automata = generaAC evolucionCifradoWolfram (regla 30) inicia
    let tamAutomata = length automata
    let numAleatorio = generaAleatorio semilla 0 tamAutomata
    let numAleatorioComienzo = tamAutomata - tamTextoTransformado
    let indices = [numAleatorioComienzo..genericLength automata-1]
    let filasAutomata = [c | (c,i)<-zip automata [0..genericLength automata-1], i `elem` indices]
    let clave = [obtieneCeldaCentral x | x<-filasAutomata]
    let operacionXOR = xorl clave textoTransformado
    let operacionXOR' = xorl clave operacionXOR
    let textoDescifrado = descodifica operacionXOR'
    let control = texto == textoDescifrado
    if control then do
        imprime "El texto se ha cifrado y descifrado correctamente."
    else do
        imprime "Algo ha fallado en el proceso de cifrado/descifrado."
    return textoDescifrado
    where
        obtieneCeldaCentral a = head [x | (x,i) <- zip a [0..(length a - 1)], i == div (length a) 2]
