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

    {----------------------------------------------------------------------
                            Versión simplificada
    ----------------------------------------------------------------------}

reglaReversible :: Int -> Int -> Int
reglaReversible r1 radioVecindad = 2^(2*radioVecindad+1) - r1 - 1

inicializaACSegundoOrden :: Int -> [Int] -> [Int] -> CycleSO Int
inicializaACSegundoOrden n datos1 datos2 = CycleSO {nCeldas=n, pasado=datos1, presente=datos2}

versionSimplificadaEncriptado :: Int -> CycleSO Int -> Int -> Int -> [[Int]]
versionSimplificadaEncriptado clave inicial regla pasos = [datosCifrados, datosRes]
    where
        automata = generaACSO regla pasos inicial (nCeldas inicial)
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
        datosCifrados = cabeza datos
        datosRes = xorl claveFormaLista datosResiduales
        inicial = inicializaACSegundoOrden (tamLista datosResiduales) datosRes datosCifrados
        reglaRev = reglaReversible regla 1                                                      -- radio de vecindad: 1
        automata = generaACSO reglaRev pasos inicial (nCeldas inicial)
        tamAutomata = tamLista automata
        datosAleatorios = ultimo automata
        descifrado = obtieneSubLista automata (tamAutomata - 2)


    {----------------------------------------------------------------------
                        Versión más segura de Bouvry 
    ----------------------------------------------------------------------}


-- CAL (radio 2), CAR (radio 2), CAC (de radio 3) CAS (radio 2)

rondaDatosIniciales :: CycleSO Int -> Int -> Int -> Int -> [CycleSO Int]
rondaDatosIniciales inicial regla radio pasos = ejecutaACSO' regla radio pasos inicial

rondaTransformaciones :: CycleSO Int -> Int -> Int -> Int -> [CycleSO Int]
rondaTransformaciones inicial regla radio pasos = undefined

