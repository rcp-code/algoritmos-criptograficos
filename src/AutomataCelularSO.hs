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


    {----------------------------------------------------------------------
                Segundo método: usando el tipo Cycle (CycleSO)
    ----------------------------------------------------------------------}

-- Extracción de datos:

extraePasado :: CycleSO Int -> [Int]
extraePasado (CycleSO nc pasado _) =  pasado

extraePresente :: CycleSO Int -> [Int]
extraePresente (CycleSO nc _ presente) = presente

vistaPlana' :: CycleSO Int -> [Int]
vistaPlana' (CycleSO nc pasado presente) = pasado

-- Aplicación de las reglas:

-- aplicaReglaSO' :: Int -> [Int] -> [Int] -> [Int]
-- aplicaReglaSO' r anteriores celdas =
--     primeraCelda : [abs $ AC.regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) - ant | (i, ant) <- L.zip [1..L.length celdas - 2] subListaCeldasAnteriores] L.++ [ultimaCelda]
--     where 
--         subLista = L.tail anteriores
--         subListaAnteriores = L.init subLista
--         subListaCeldasAnteriores = L.init $ L.tail subListaAnteriores -- se le quitan el primero y el último para quedarnos solo con las celdas que ejercerían como centrales en la lista por comprensión
--         primeraCelda = abs $ AC.regla r (ultimo celdas) (cabeza celdas) (cabeza (L.tail celdas)) - cabeza anteriores
--         ultimaCelda = abs $ AC.regla r (ultimo (L.init celdas)) (ultimo celdas) (cabeza celdas) - ultimo anteriores

aplicaReglaSO' :: Int -> [Int] -> [Int] -> [Int]
aplicaReglaSO' r anteriores celdas =
    primeraCelda : [abs $ AC.regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) - (anteriores !! i) | i <- [1..L.length celdas - 2]] L.++ [ultimaCelda]
    where 
        primeraCelda = abs $ AC.regla r (ultimo celdas) (cabeza celdas) (cabeza (L.tail celdas)) - cabeza anteriores
        ultimaCelda = abs $ AC.regla r (ultimo (L.init celdas)) (ultimo celdas) (cabeza celdas) - ultimo anteriores


aplicaRegla' :: Int -> [Int] -> [Int]
aplicaRegla' reg celdas = primeraCelda : [AC.regla reg (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) | i <- [1..L.length celdas - 2]] L.++ [ultimaCelda]
    where 
        primeraCelda = AC.regla reg (ultimo celdas) (cabeza celdas) (cabeza (L.tail celdas))
        ultimaCelda = AC.regla reg (ultimo (L.init celdas)) (ultimo celdas) (cabeza celdas)

-- Inicialización:

inicializa :: Int -> Int -> [Int] -> CycleSO Int
inicializa n reg lista = CycleSO {nCeldas=n, pasado=vecindad, presente=futuraVecindad}
    where 
        vecindad = L.take n lista
        poss = (0, 1, 2)
        futuraVecindad = aplicaRegla' reg vecindad

-- Ejecución:

unPaso :: Int -> CycleSO Int -> CycleSO Int 
unPaso reg ciclo@(CycleSO _ pasado presente) = CycleSO {pasado=nuevoPasado, presente=nuevaVecindad}
    where 
        pas = extraePasado ciclo
        pres = extraePresente ciclo
        nuevoPasado = pres
        nuevaVecindad = aplicaReglaSO' reg pas pres

ejecutaACSO :: Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO regla pasos ciclo = ejecuta regla pasos ciclo [] 

ejecuta :: Int -> Int -> CycleSO Int -> [CycleSO Int] -> [CycleSO Int]
ejecuta regla pasos ciclo aux 
    | pasos>0 = ejecuta regla (pasos-1) nuevoCiclo (nuevoCiclo:ciclo:aux)
    | otherwise = L.reverse aux
    where 
        nuevoCiclo = unPaso regla ciclo

generaACSO :: Int -> Int -> CycleSO Int -> Int -> [[Int]]
generaACSO regla pasos ini ancho = vistaPlana' <$> ejecutaACSO regla pasos ini

muestraACSO ::Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO n regla pasos ini = P.mapM_ putStrLn $ L.take n automata
    where 
        automata = fmap muestra . vistaPlana' <$> ejecutaACSO regla pasos ini
        muestra 0 = ' '
        muestra 1 = '*'
    
    