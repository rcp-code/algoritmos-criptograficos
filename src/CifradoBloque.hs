module CifradoBloque where


import Tipos
import AutomataCelularSO as ACSO
import UtilGeneral
import UtilCripto
---
import Data.Bits
import Data.Char
import Data.List

    {----------------------------------------------------------------------
                Cifrado de bloque basado en AC de segundo orden
    ----------------------------------------------------------------------}

datosAleatorios :: Int -> [Int]
datosAleatorios semilla = concat (cambioABase2Lista (generaAleatoriosL semilla))

xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

-- XOR aplicado a dos listas, número a número (bit a bit)
xorl :: [Int] -> [Int] -> [Int]
xorl datos1 datos2 = [xor2 x y | (x,y)<-zip datos1 datos2]
    where
        xor2 a b
            | xor' x y = 1
            | otherwise = 0
            where
                x = a==1
                y = b==1

divideEnDosSubBloques :: [Int] -> [[Int]]
divideEnDosSubBloques [] = error "El bloque de datos esta vacio, no se puede realizar la particion."
divideEnDosSubBloques bloque = [primeraSublista, segundaSublista]
    where
        tam = tamLista bloque - 1
        tamParticion = div tam 2
        primeraSublista = slicing bloque 0 tamParticion
        segundaSublista = slicing bloque (tamParticion + 1) tam

reglaReversible :: Int -> Int -> Int
reglaReversible r1 radioVecindad = 2^(2*radioVecindad+1) - r1 - 1

    {----------------------------------------------------------------------
                            Versión simplificada
    ----------------------------------------------------------------------}

inicializaACSegundoOrden :: Int -> [Int] -> [Int] -> CycleSO Int
inicializaACSegundoOrden n datos1 datos2 = CycleSO {nCeldas=n, pasado=datos1, presente=datos2}

versionSimplificadaEncriptado :: Int -> CycleSO Int -> Int -> Int -> [[Int]]
versionSimplificadaEncriptado clave inicial regla pasos = [datosCifrados, datosRes]
    where
        automata = generaACSO regla pasos inicial
        tamAutomata = tamLista automata
        datosResiduales = ultimo automata
        datosCifrados = obtieneSubLista automata (tamAutomata - 2)
        claveFormaLista = digitos clave
        datosRes = xorl claveFormaLista datosResiduales

versionSimplificadaDesencriptado :: Int -> [[Int]] -> Int -> Int -> [[Int]]
versionSimplificadaDesencriptado clave datos regla pasos = [descifrado, datosAleatorios]
    where
        claveFormaLista = digitos clave
        datosResiduales = ultimo datos
        datosCifrados = primero datos
        datosRes = xorl claveFormaLista datosResiduales
        inicial = inicializaACSegundoOrden (tamLista datosResiduales) datosRes datosCifrados
        reglaRev = reglaReversible regla 1                                                      -- radio de vecindad: 1
        automata = generaACSO reglaRev pasos inicial
        tamAutomata = tamLista automata
        datosAleatorios = ultimo automata
        descifrado = obtieneSubLista automata (tamAutomata - 2)


    {----------------------------------------------------------------------
                        Versión más segura de Bouvry 
    ----------------------------------------------------------------------}

-- Desplaza bits n veces a derecha o izquierda
desplazaBits :: [Int] -> Int -> Char -> [Int]
desplazaBits qn n despl
    | despl=='L' = drop n qn ++ take n qn
    | despl=='R' = drop m qn ++ take m qn
    | otherwise = error "El desplazamiento no es correcto."
    where 
        m = length qn - n

-- Tamaño de bloque de 16 bits y radio de vecindad 2
cas :: Int -> Int -> [Int] -> [Int] -> Int
cas regla pasos datos1 datos2 = ns
    where
        inicial = inicializacion 16 datos1 datos2
        automata = generaACSO' regla 0 pasos inicial            -- regla radio pasos ini
        p = div pasos 2
        sublista1 = slicing automata 2 p
        sublista2 = slicing automata (p+1) pasos
        ns1 = concat [c | (c,i)<-zip sublista1 [1..tamLista sublista1], i==div (tamLista automata) 2]
        ns2 = concat [c | (c,i)<-zip sublista2 [1..tamLista sublista2], i==div (tamLista automata) 2]
        ns = deListaBinarioANum (ns1 ++ ns2)

calr :: Int -> Int -> [Int] -> [Int] -> [[Int]]
calr regla pasos q0 q1 = [qn_1L, qnL]
    where
        inicial = inicializacion 32 q0 q1
        automata = generaACSO' regla 2 pasos inicial
        qnL = automata !! (tamLista automata - 1)
        qn_1L = automata !! (tamLista automata - 2)

cac :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
cac regla pasos qn_1R qL qn_1L qnR = [qn_1C, qnC]
    where
        q = qn_1R ++ qn_1L
        q' = qL ++ qnR
        inicial = inicializacion 64 q q'
        automata = generaACSO' regla 3 pasos inicial
        qn_1C = automata !! (tamLista automata - 1)
        qnC = automata !! (tamLista automata - 2)

{--
    Key:
        - 0-31: CAL rule
        - 32-63: CAR rule
        - 64-191: CAC rule
        - 192-223: CAS rule -}

rondaDatosIniciales :: CycleSO Int -> Int -> Int -> Int -> [[Int]]
rondaDatosIniciales inicial regla radio pasos = generaACSO' regla radio pasos inicial

rondaTransformaciones :: CycleSO Int -> Int -> Int -> Int -> [[Int]]
rondaTransformaciones inicial regla radio pasos = undefined