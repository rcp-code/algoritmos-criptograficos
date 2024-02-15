module Constantes where

    {----------------------------------------------------------------------
                        Definición de "constantes"
    ----------------------------------------------------------------------}

import Data.List as L
import Prelude as P

    {----------------------------------------------------------------------
                        Constantes para cifrado
    ----------------------------------------------------------------------}

caracteres :: String
caracteres = ' ' : ['a'..'z'] P.++ ['A'..'Z'] P.++ ['0'..'9'] P.++ ['.',',']

numeros :: [Int]
numeros = [0..n]
    where n = L.genericLength caracteres-1

asociaciones :: [(Char, Int)]
asociaciones = L.zip caracteres numeros

    {----------------------------------------------------------------------
                    Constantes de autómatas celulares
    ----------------------------------------------------------------------}

minCeldas :: Int
minCeldas = 1000

maxCeldas :: Int
maxCeldas = 5000

numCeldas :: Int
numCeldas = 55

-- Configuración inicial con una sola celda activa
configuracionInicial :: [Int]
configuracionInicial = [1]

-- Número de pasos
numPasos :: Int
numPasos = 255