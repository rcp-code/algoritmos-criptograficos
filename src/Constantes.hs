module Constantes where

    {----------------------------------------------------------------------
                        Definición de "constantes"
    ----------------------------------------------------------------------}

import Data.List

    {----------------------------------------------------------------------
                        Constantes para cifrado
    ----------------------------------------------------------------------}

caracteres :: String
caracteres = ' ' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.',',']

numeros :: [Int]
numeros = [10..]

asociaciones :: [(Char, Int)]
asociaciones = zip caracteres numeros

    {----------------------------------------------------------------------
                    Constantes de autómatas celulares
    ----------------------------------------------------------------------}

minCeldas :: Int
minCeldas = 150

maxCeldas :: Int
maxCeldas = 5000

numCeldas :: Int
numCeldas = 81

numPasos :: Int
numPasos = 500

reglaAC :: Int
reglaAC = 30

pasosEvolucion :: Int
pasosEvolucion = 10

numPasosCALR, numPasosCAC, numPasosCAS :: Int
numPasosCALR = 5
numPasosCAC = 7
numPasosCAS = 10

numPasosCifrado :: Int
numPasosCifrado = 10

numBits :: Int
numBits = 64