{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
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


-- Extracción de datos:

extraePasado :: CycleSO Int -> [Int]
extraePasado (CycleSO nc pasado _) =  pasado

extraePresente :: CycleSO Int -> [Int]
extraePresente (CycleSO nc _ presente) = presente

vistaPlana' :: CycleSO Int -> [Int]
vistaPlana' (CycleSO nc pasado presente) = pasado

-- Aplicación de las reglas:

-- Radio de vecindad=1

aplicaReglaSO :: Int -> [Int] -> [Int] -> [Int]
aplicaReglaSO r anteriores celdas =
    primeraCelda : [abs $ AC.regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) - (anteriores !! i) | i <- [1..L.length celdas - 2]] L.++ [ultimaCelda]
    where
        primeraCelda = abs $ AC.regla r (ultimo celdas) (cabeza celdas) (cabeza (L.tail celdas)) - cabeza anteriores
        ultimaCelda = abs $ AC.regla r (ultimo (L.init celdas)) (ultimo celdas) (cabeza celdas) - ultimo anteriores


aplicaRegla :: Int -> [Int] -> [Int]
aplicaRegla reg celdas = primeraCelda : [AC.regla reg (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) | i <- [1..L.length celdas - 2]] L.++ [ultimaCelda]
    where
        primeraCelda = AC.regla reg (ultimo celdas) (cabeza celdas) (cabeza (L.tail celdas))
        ultimaCelda = AC.regla reg (ultimo (L.init celdas)) (ultimo celdas) (cabeza celdas)

-- Inicialización:

inicializa :: Int -> Int -> [Int] -> CycleSO Int
inicializa n reg lista = CycleSO {nCeldas=n, pasado=vecindad, presente=futuraVecindad}
    where
        vecindad = L.take n lista
        futuraVecindad = aplicaRegla reg vecindad

-- Ejecución:

unPaso :: Int -> CycleSO Int -> CycleSO Int
unPaso reg ciclo@(CycleSO _ pasado presente) = CycleSO {pasado=nuevoPasado, presente=nuevaVecindad}
    where
        pas = extraePasado ciclo
        pres = extraePresente ciclo
        nuevoPasado = pres
        nuevaVecindad = aplicaReglaSO reg pas pres

ejecutaACSO :: Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO regla pasos ciclo = ejecuta regla pasos ciclo []

ejecuta :: Int -> Int -> CycleSO Int -> [CycleSO Int] -> [CycleSO Int]
ejecuta regla pasos ciclo aux
    | pasos>0 = ejecuta regla (pasos-1) nuevoCiclo (nuevoCiclo:ciclo:aux)
    | otherwise = aux
    where
        nuevoCiclo = unPaso regla ciclo

generaACSO :: Int -> Int -> CycleSO Int -> Int -> [[Int]]
generaACSO regla pasos ini ancho = vistaPlana' <$> ejecutaACSO regla pasos ini

-- Muestra el AC:

muestraACSO ::Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO n regla pasos ini = P.mapM_ putStrLn $ L.take n automata
    where
        automata = fmap muestra . vistaPlana' <$> ejecutaACSO regla pasos ini
        muestra 0 = ' '
        muestra 1 = '*'

    {------------------------------------------------------------------------------------------
                            Autómatas celulares de segundo orden con radio 
                                        de vecindad mayor que 1
    ------------------------------------------------------------------------------------------}

aplicaReglaSO' :: Int -> Int -> [Int] -> [Int] -> [Int]
aplicaReglaSO' r radio anteriores celdas =
    primeraCelda : [abs $ AC.regla r (obtenerCelda i (tamLista celdas) celdas) (celdas !! i) (obtenerCelda (i+1) radio celdas) - (anteriores !! i) | i <- [radio..L.length celdas - radio - 1]] L.++ [ultimaCelda]
    where
        celdas' = celdas
        primeraCelda = abs $ AC.regla r (obtenerCelda (L.length celdas' - 1) radio celdas') (cabeza celdas')  (cabeza (L.drop 1 celdas')) - cabeza anteriores
        ultimaCelda = abs $ AC.regla r (obtenerCelda (L.length celdas - radio - 2) radio celdas) (ultimo celdas) (cabeza celdas) - ultimo anteriores

obtenerCelda :: Int -> Int -> [Int] -> Int
obtenerCelda pos radio celdas
  | pos<radio = cabeza celdas
  | pos>=L.length celdas - radio = ultimo celdas
  | otherwise = celdas !! pos


aplicaRegla' :: Int -> Int -> [Int] -> [Int]
aplicaRegla' reg radio celdas = primeraCelda : [AC.regla reg (obtenerCelda i (tamLista celdas) celdas) (celdas !! i) (obtenerCelda (i+1) radio celdas) | i <- [radio..L.length celdas - radio - 1]] L.++ [ultimaCelda]
    where
        celdas' = celdas
        primeraCelda = AC.regla reg (obtenerCelda (L.length celdas' - 1) radio celdas') (cabeza celdas')  (cabeza (L.drop 1 celdas'))
        ultimaCelda = AC.regla reg (obtenerCelda (L.length celdas - radio - 2) radio celdas) (ultimo celdas) (cabeza celdas)

unPaso' :: Int -> Int -> CycleSO Int -> CycleSO Int
unPaso' reg radio ciclo@(CycleSO _ pasado presente) = CycleSO {pasado=nuevoPasado, presente=nuevaVecindad}
    where
        pas = extraePasado ciclo
        pres = extraePresente ciclo
        nuevoPasado = pres
        nuevaVecindad = aplicaReglaSO' reg radio pas pres

ejecutaACSO' :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO' regla radio pasos ciclo = ejecuta' regla radio pasos ciclo []

ejecuta' :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int] -> [CycleSO Int]
ejecuta' regla radio pasos ciclo aux
    | pasos>0 = ejecuta' regla radio (pasos-1) nuevoCiclo (nuevoCiclo:ciclo:aux)
    | otherwise = aux
    where
        nuevoCiclo = unPaso' regla radio ciclo

generaACSO' :: Int -> Int -> Int -> CycleSO Int -> [[Int]]
generaACSO' regla radio pasos ini = vistaPlana' <$> ejecutaACSO' regla radio pasos ini

muestraACSO' :: Int -> Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO' n regla radio pasos ini = P.mapM_ putStrLn $ L.take n automata
    where
        automata = fmap muestra . vistaPlana' <$> ejecutaACSO' regla radio pasos ini
        muestra 0 = ' '
        muestra 1 = '*'