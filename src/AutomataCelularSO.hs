{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module AutomataCelularSO where

    {----------------------------------------------------------------------
                    Autómatas celulares de segundo orden
    ----------------------------------------------------------------------}

import UtilGeneral
import Tipos
import AutomataCelular1D as AC
import Constantes

import Data.Vector as V
import Data.List as L
import Control.Comonad
import Data.InfList as Inf
import Prelude as P
import Data.Bits
import UtilIO


-- Configuración inicial aleatoria
inicialAleatorio :: Int -> [Int]
inicialAleatorio semilla = L.concat $ cambioABase2Lista (generaAleatoriosL semilla)

-- Aplica una regla específica a una lista de células
aplicaRegla :: Int -> [Int] -> [Int]
aplicaRegla reg celdas = [AC.regla reg (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) | i <- [1..L.length celdas - 2]]

listaVecindades :: [Int] -> [(Int, Int, Int)]
listaVecindades celdas = [(celdas !! (i - 1), celdas !! i, celdas !! (i + 1)) | i <- [1..L.length celdas - 2]]

celdaCentralPosVecindad :: [(Int, Int, Int)] -> [Int]
celdaCentralPosVecindad vecindad = [c | (_,c,_) <- vecindad]

-- Aplica la regla especificada teniendo en cuenta el estado anterior de la celda (para autómatas reversibles)
aplicaReglaSO :: Int -> [Int] -> [Int] -> [Int]
aplicaReglaSO r anteriores celdas =
    [abs $ AC.regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) - ant | (i, ant) <- L.zip [1..L.length celdas - 2] anteriores]

-- Genera una configuración inicial y a partir de esta, el autómata celular evoluciona un número de pasos establecido
generaAutomata :: Int -> Int -> Int -> IO ()
generaAutomata regla pasos anchoFila = do
    let inicial = L.replicate (anchoFila `div` 2) 1 L.++ [1] L.++ L.replicate (anchoFila `div` 2) 0
        reglaAplicadaCeldas = aplicaRegla regla inicial
    pintaCeldas reglaAplicadaCeldas pasos
    where
        pintaCeldas celdas 0 = return ()
        pintaCeldas celdas n = do
            putStrLn $ L.concatMap show celdas
            let siguientesCeldas = aplicaRegla regla celdas
            pintaCeldas siguientesCeldas (n - 1)

-- Genera una configuración inicial aleatoria y a partir de esta evoluciona cierto número de pasos el autómata celular:
generaAutomataAleatorio :: Int -> Int -> Int -> IO ()
generaAutomataAleatorio regla pasos anchoFila = do
    semilla <- now
    let inicial = L.take anchoFila $ inicialAleatorio semilla
        reglaAplicada = aplicaRegla regla inicial
    pintaCeldas reglaAplicada pasos
    where
        pintaCeldas celdas 0 = return ()
        pintaCeldas celdas n = do
            putStrLn $ L.concatMap show celdas
            let siguientesCeldas = aplicaRegla regla celdas
            pintaCeldas siguientesCeldas (n - 1)

-- Otra versión de generación de autómata celular aleatorio (genera las filas, no las pinta como en los casos anteriores):
-- generaAutomataAleatorio' :: Int -> Int -> Int -> IO [[Int]]
-- generaAutomataAleatorio' regla pasos anchoFila = do
--     semilla <- now
--     let inicial = L.take anchoFila $ inicialAleatorio semilla
--         reglaAplicada = aplicaRegla regla inicial
--     generaFila reglaAplicada pasos []
--     where
--         generaFila celdas 0 acumulado = return $ L.reverse $ L.filter (/=[]) acumulado
--         generaFila celdas n acumulado = do
--             let filaActual = L.map fromEnum celdas
--             let siguientesCeldas = aplicaRegla regla celdas
--             generaFila siguientesCeldas (n - 1) (filaActual : acumulado)

-- Otra versión de la anterior, en este caso se tiene en cuenta también el instante anterior
generaAutomataAleatorio' :: Int -> Int -> Int -> IO [[Int]]
generaAutomataAleatorio' regla pasos anchoFila = do
    semilla <- now
    let inicial = L.take anchoFila $ inicialAleatorio semilla
        reglaAplicada = aplicaRegla regla inicial
    generaFila reglaAplicada pasos []
    where
        generaFila celdas 0 acumulado = return $ L.reverse $ L.filter (/=[]) acumulado
        generaFila celdas n acumulado = do
            let filaActual = L.map fromEnum celdas
            if tamLista acumulado>1
                then do
                    let anterior = ultimo acumulado
                    let siguientesCeldasR = aplicaReglaSO regla anterior celdas
                    generaFila siguientesCeldasR (n - 1) (filaActual : acumulado)
            else do
                let siguientesCeldas = aplicaRegla regla celdas
                generaFila siguientesCeldas (n - 1) (filaActual : acumulado)
            --generaFila siguientesCeldas (n - 1) (filaActual : acumulado)

-- main :: IO [[Int]]
-- main = generaAutomataAleatorio' 30 51 51

pruebaAutomataSO :: IO ()
pruebaAutomataSO = do
    automata <- generaAutomataAleatorio' 30 numPasos numPasos
    let vecindades = [(obtieneElemento x (abs (div (tamLista x) 2) - 1), elementoCentral x, obtieneElemento x (abs (div (tamLista x) 2) + 1)) | x<-L.tail automata, x /= [], abs (div (tamLista x) 2) + 1 < tamLista x-1]
    print vecindades
    let listaCentros' = [elementoCentral x | x<-L.tail automata, x /= []]
    let primerElemento = cabeza $ cabeza automata
    let listaCentros = primerElemento : listaCentros'
    print listaCentros
    let numero = deListaBinarioANum listaCentros
    print numero