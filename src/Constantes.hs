module Constantes where

    {----------------------------------------------------------------------
                        Definición de "constantes"
    ----------------------------------------------------------------------}

import Data.List
import UtilIO

    {----------------------------------------------------------------------
                        Constantes para cifrado
    ----------------------------------------------------------------------}

caracteres :: String
caracteres = ' ' : letras ++ ['0'..'9'] ++ caracteresEspeciales

letras :: String
letras = ['a'..'z'] ++ ['A'..'Z']

caracteresEspeciales :: String
caracteresEspeciales = ".:,;!?@$%&|~/()=[]<>-_+*^ºª\n\t\"\'"

abecedario :: String
abecedario = ['A'..'Z']

numeros :: [Int]
numeros = [10..]

asociaciones :: [(Char, Int)]
asociaciones = zip caracteres numeros

-- Numero correspondiente al primer caracter en asociaciones
primerCaracterAsociaciones :: Int
primerCaracterAsociaciones = snd $ head asociaciones

-- Numero correspondiente al ultimo caracter en asociaciones
ultimoCaracterAsociaciones :: Int
ultimoCaracterAsociaciones = snd $ last asociaciones

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

-- pasosEvolucion :: Int
-- pasosEvolucion = 10

numPasosCALR :: Int
numPasosCALR = 19 --4

numPasosCAC :: Int 
numPasosCAC = 17 --4

numPasosCAS :: Int
numPasosCAS = 5     --NO CAMBIAR BAJO NINGÚN CONCEPTO

numeroRondas :: Int
numeroRondas = 15

numBits :: Int
numBits = 64

evolucionCifradoWolfram :: Int
evolucionCifradoWolfram = 1500