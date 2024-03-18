{-# OPTIONS_GHC -Wno-missing-fields #-}
module AutomataCelular where

    {----------------------------------------------------------------------
                  Incluye las funciones necesarias para 
                      los autómatas celulares.
    ----------------------------------------------------------------------}

import Tipos
import Constantes
import UtilGeneral
import Data.Functor
import Prelude as P
import Control.Comonad
import System.Random
import Data.InfList as Inf
import Data.List as L
import Data.Vector as V
import Data.Matrix as M


    {-------------------------------------------------------------------------
                      Tratamiento de autómatas celulares
    -------------------------------------------------------------------------}
instance Comonad Cycle where
  extract (Cycle _ _ cen _) = cen
  duplicate cel@(Cycle n _ _ _) = deListaACiclo $ Inf.take n $ Inf.iterate cambia cel
    where
      cambia (Cycle n _ cen (der:::ds)) = Cycle n cen der ds

--Transforma un ciclo en una lista 
vista :: Cycle a -> [a]
vista (Cycle n _ cen der) = Inf.take n (cen:::der)

--Transforma una lista en un ciclo
deListaACiclo :: [a] -> Cycle a
deListaACiclo []  = let a = a in Cycle 0 a a (Inf.repeat a)      
deListaACiclo lista = let cel:::der = Inf.cycle lista in Cycle (P.length lista) (P.last lista) cel der

    {-------------------------------------------------------------------------
                      Creación de los autómatas celulares
    -------------------------------------------------------------------------}

regla :: (Integral a, Integral b) => a -> b -> b -> b -> a
regla r izq cen der = r `div` (2^(4*izq + 2*cen + der)) `mod` 2

ejecutaRegla :: (a1 -> a1 -> a1 -> a2) -> Cycle a1 -> a2
ejecutaRegla regla (Cycle _ izq cen (der:::_)) = regla izq cen der

ejecutaAC :: (Eq a, Integral a) => (a -> a -> a -> a) -> Cycle a -> InfList (Cycle a)
ejecutaAC regla = Inf.iterate (=>> ejecutaRegla regla)    -- => extiende con los argumentos intercambiados

    {-----------------------------------------------------------------------------------
                              Inicialización del AC
    ------------------------------------------------------------------------------------}

-- Inicializa el AC (puede ser aleatorio o no)
iniciaAC :: (Integral a, Num a) => Int -> [a] -> Cycle a
iniciaAC numCels lista = deListaACiclo $ centro $ incluyeVecinosDer numCels lista
  where
    incluyeVecinosDer numCels lista = P.take numCels $ lista P.++ P.repeat 0
    centro = Inf.take numCels . Inf.drop (numCels `div` 2+1) . Inf.cycle

    {-----------------------------------------------------------------------------------
                                  Generación del AC
    ------------------------------------------------------------------------------------}

generaAC :: (Integral a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> [[a]]
generaAC n regla ini = Inf.take n automata
  where
    automata = vista <$> ejecutaAC regla ini

    {-----------------------------------------------------------------------------------
                                    Muestra el AC
    ------------------------------------------------------------------------------------}

-- Solo para fines de comprobación para saber si el AC funciona bien
muestraAC :: (Eq a, Integral a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> IO ()
muestraAC n regla ini = P.mapM_ putStrLn $ Inf.take n automata
  where
    automata = fmap muestra . vista <$> ejecutaAC regla ini   -- <$> - sinónimo infijo de fmap, que reemplaza todas las ubicaciones en la entrada por el mismo valor
    muestra 0 = ' '
    muestra 1 = '*'


    {-----------------------------------------------------------------------------------
                            Autómatas de segundo orden
    ------------------------------------------------------------------------------------}

-- Extracción y transformación de datos:

extraePasado :: CycleSO Int -> [Int]
extraePasado (CycleSO nc pasado _) =  pasado

extraePresente :: CycleSO Int -> [Int]
extraePresente (CycleSO nc _ presente) = presente

vistaPlana :: CycleSO Int -> [Int]
vistaPlana (CycleSO nc pasado presente) = pasado

-- inicializa :: Int -> Int -> Int -> [Int] -> CycleSO Int
-- inicializa n reg radioVecindad lista = CycleSO {nCeldas=n, pasado=vecindad, presente=futuraVecindad}
--     where
--         vecindad = L.take n lista
--         futuraVecindad = aplicaRegla reg radioVecindad vecindad

-- Datos iniciales para el autómata de segundo orden
inicializacion :: Int -> [Int] -> [Int] -> CycleSO Int
inicializacion n lista1 lista2 = CycleSO {nCeldas=n, pasado=lista1, presente=lista2}

-- Cálculo de la regla con radio de vecindad 2:
reglaVecindadDos :: Int -> Int -> Int -> Int -> Int -> Int -> Int
reglaVecindadDos r izq2 izq1 cen der1 der2 = r `div` (2^(5*izq2 + 4*izq1 + 3*cen + 2*der1 + der2)) `mod` 2

-- Cálculo de la regla con radio de vecindad 3:
reglaVecindadTres :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
reglaVecindadTres r izq3 izq2 izq1 cen der1 der2 der3 = r `div` (2^(7* izq3 + 6*izq2 + 5*izq1 + 4*cen + 3*der1 + 2*der2 + der1)) `mod` 2

-- Aplica regla de segundo orden para vecindades 1, 2 y 3:
aplicaReglaSO :: Int -> Int -> [Int] -> [Int] -> [Int]
aplicaReglaSO r radio anteriores celdas
    | radio==1 = primeraCelda1 : [abs $ regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) - (anteriores !! i) | i <- [1..L.length celdas - 2]] L.++ [ultimaCelda1]
    | radio==2 = primeraCelda2 : segundaCelda2 : [abs $ reglaVecindadDos r (celdas !! (i - 2)) (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) (celdas !! (i + 2)) - (anteriores !! i) | i <- [2..L.length celdas - 4]] L.++ [penultimaCelda2] L.++ [ultimaCelda2]
    | radio==3 = primeraCelda3 : segundaCelda3 : terceraCelda3 : [abs $ reglaVecindadTres r (celdas !! (i - 3)) (celdas !! (i - 2)) (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) (celdas !! (i + 2)) (celdas !! (i + 3)) - (anteriores !! i) | i <- [3..L.length celdas - 6]] L.++ [antepenultimaCelda3] L.++ [penultimaCelda2] L.++ [ultimaCelda3]
    | otherwise = error "El radio definido no está entre 1 y 3."
    where
        -- Primera y última celda para radio 1 de vecindad:
        primeraCelda1 = abs $ regla r (ultimo celdas) (primero celdas) (primero (L.tail celdas)) - primero anteriores
        ultimaCelda1 = abs $ regla r (ultimo (L.init celdas)) (ultimo celdas) (primero celdas) - ultimo anteriores
        -- Primera, segunda, penúltima y última celda para radio 2 de vecindad:
        primeraCelda2 = abs $ reglaVecindadDos r (ultimo celdas) (primero celdas) (primero (L.tail celdas)) (L.tail celdas !! 1) (L.tail celdas !! 2) - primero (L.tail anteriores)
        segundaCelda2 = abs $ reglaVecindadDos r (primero celdas) (primero (L.tail celdas)) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3) - L.tail anteriores !! 1
        penultimaCelda2 = abs $ reglaVecindadDos r (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo (L.init celdas)) (ultimo celdas) (primero celdas) - ultimo (L.init anteriores)
        ultimaCelda2 = abs $ reglaVecindadDos r (L.init celdas !! (tamLista celdas - 3)) (ultimo (L.init celdas)) (ultimo celdas) (primero celdas) (primero (L.tail celdas)) - ultimo anteriores
        -- Celdas de la frontera para radio 2 de vecindad:
        primeraCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2) - primero anteriores
        segundaCelda3 = abs $ reglaVecindadTres r (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3) - primero (L.tail anteriores)
        terceraCelda3 = abs $ reglaVecindadTres r (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3) (L.tail celdas !! 4) - L.tail anteriores !! 1
        antepenultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 6)) (L.init celdas !! (tamLista celdas - 5)) (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) - L.init anteriores !! (tamLista anteriores - 3)
        penultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 5)) (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) - ultimo (L.init anteriores)
        ultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) - ultimo anteriores

-- Aplica la regla (normal) especificada para vecindades 1, 2 y 3 (no es necesario):
-- aplicaRegla :: Int -> Int -> [Int] -> [Int]
-- aplicaRegla r radio celdas
--   | radio==1 = primeraCelda1 : [abs $ regla r (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) | i <- [radio..L.length celdas - radio - 1]] L.++ [ultimaCelda1]
--   | radio==2 = primeraCelda2 : segundaCelda2 : [abs $ reglaVecindadDos r (celdas !! (i - 2)) (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) (celdas !! (i + 2)) | i <- [2..L.length celdas - 4]] L.++ [penultimaCelda2] L.++ [ultimaCelda2]
--   | radio==3 = primeraCelda3 : segundaCelda3 : terceraCelda3 : [abs $ reglaVecindadTres r (celdas !! (i - 3)) (celdas !! (i - 2)) (celdas !! (i - 1)) (celdas !! i) (celdas !! (i + 1)) (celdas !! (i + 2)) (celdas !! (i + 3)) | i <- [3..L.length celdas - 6]] L.++ [antepenultimaCelda3] L.++ [penultimaCelda2] L.++ [ultimaCelda3]
--     where
--         -- Primera y última celda para radio 1 de vecindad:
--         primeraCelda1 = abs $ regla r (ultimo celdas) (primero celdas) (primero (L.tail celdas))
--         ultimaCelda1 = abs $ regla r (ultimo (L.init celdas)) (ultimo celdas) (primero celdas)
--         -- Primera, segunda, penúltima y última celda para radio 2 de vecindad:
--         primeraCelda2 = abs $ reglaVecindadDos r (ultimo celdas) (primero celdas) (primero (L.tail celdas)) (L.tail celdas !! 1) (L.tail celdas !! 2)
--         segundaCelda2 = abs $ reglaVecindadDos r (primero celdas) (primero (L.tail celdas)) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3)
--         penultimaCelda2 = abs $ reglaVecindadDos r (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo (L.init celdas)) (ultimo celdas) (primero celdas)
--         ultimaCelda2 = abs $ reglaVecindadDos r (L.init celdas !! (tamLista celdas - 3)) (ultimo (L.init celdas)) (ultimo celdas) (primero celdas) (primero (L.tail celdas))
--         -- Celdas de la frontera para radio 2 de vecindad:
--         primeraCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2)
--         segundaCelda3 = abs $ reglaVecindadTres r (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3)
--         terceraCelda3 = abs $ reglaVecindadTres r (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1) (L.tail celdas !! 2) (L.tail celdas !! 3) (L.tail celdas !! 4)
--         antepenultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 6)) (L.init celdas !! (tamLista celdas - 5)) (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas)
--         penultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 5)) (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas)
--         ultimaCelda3 = abs $ reglaVecindadTres r (L.init celdas !! (tamLista celdas - 4)) (L.init celdas !! (tamLista celdas - 3)) (ultimo $ L.init celdas) (ultimo celdas) (primero celdas) (primero $ L.tail celdas) (L.tail celdas !! 1)

-- Realiza un paso de la evolución del AC de segundo orden:
unPaso :: Int -> Int -> CycleSO Int -> CycleSO Int
unPaso reg radio ciclo@(CycleSO _ pasado presente) = CycleSO {pasado=nuevoPasado, presente=nuevaVecindad}
  where
        pas = extraePasado ciclo
        pres = extraePresente ciclo
        nuevoPasado = pres
        nuevaVecindad = aplicaReglaSO reg radio pas pres

-- Ejecuta el AC de segundo orden (se realizan varios pasos en la evolución):
ejecutaACSO :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO regla radio pasos ciclo = ejecuta regla radio pasos ciclo []

ejecuta :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int] -> [CycleSO Int]
ejecuta regla radio pasos ciclo aux
    | pasos>0 = ejecuta regla radio (pasos-1) nuevoCiclo (nuevoCiclo:ciclo:aux)
    | otherwise = aux
    where
        nuevoCiclo = unPaso regla radio ciclo

-- Genera el AC de segundo orden:
generaACSO :: Int -> Int -> Int -> CycleSO Int -> [[Int]]
generaACSO regla radio pasos ini = vistaPlana <$> ejecutaACSO regla radio pasos ini

-- Muestra el AC de segundo orden:
muestraACSO ::Int -> Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO n regla radio pasos ini = P.mapM_ putStrLn $ L.take n automata
    where
        automata = fmap muestra . vistaPlana <$> ejecutaACSO regla radio pasos ini
        muestra 0 = ' '
        muestra 1 = '*'