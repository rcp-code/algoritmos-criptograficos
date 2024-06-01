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
import UtilIO
import UtilCripto



instance Comonad Cycle where
  extract (Cycle _ _ cen _) = cen
  duplicate cel@(Cycle n _ _ _) = deListaACiclo $ Inf.take n $ Inf.iterate cambia cel
    where
      cambia (Cycle n _ cen (der:::ds)) = Cycle n cen der ds

    {-------------------------------------------------------------------------
                      Tratamiento de autómatas celulares
    -------------------------------------------------------------------------}

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
generaAC n regla inicial = Inf.take n automata
  where
    automata = vista <$> ejecutaAC regla inicial

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

vista' :: CycleSO Int -> [Int]
vista' (CycleSO nc pasado presente) = presente

-- Datos iniciales para el autómata de segundo orden
inicializacion :: Int -> [Int] -> [Int] -> CycleSO Int
inicializacion n lista1 lista2 = CycleSO {nCeldas=n, pasado=lista1, presente=lista2}

reglaVecindad :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
reglaVecindad reg radio izq3 izq2 izq1 cen der1 der2 der3
  | radio==2 = (reg `div` divisorR2) `mod` 2
  | radio==3 = fromInteger (toInteger reg `div` divisorR3) `mod` 2
  | otherwise = error "El radio de vecindad no es valido."
  where
    divisorR2 = 2^(16*izq2 + 8*izq1 + 4*cen + 2*der1 + der2)
    divisorR3 = 2^(64*izq3 + 32*izq2 + 16*izq1 + 8*cen + 4*der1 + 2*der2 + der3)

aplicaReglaSegundoOrden :: Int -> Int -> [Int] -> [Int] -> [Int]
aplicaReglaSegundoOrden reg radioVecindad pasado presente
    | radioVecindad == 1 || radioVecindad == 2 || radioVecindad == 3 = primerasCeldas ++ celdasIntermedias ++ ultimasCeldas
    | otherwise = error "El radio de vecindad no es valido."
    where
      tam = length presente
      primerasCeldas = obtienePrimerasCeldas pasado presente 0 reg radioVecindad
      celdasIntermedias = [obtieneNuevaCeldaActual pasado presente i reg radioVecindad | i <- [radioVecindad..(tam-radioVecindad-1)]]
      ultimasCeldas = obtieneUltimasCeldas pasado presente (tam-1) reg radioVecindad

--Función auxiliar para obtener las nuevas celdas a partir de la regla (celdas intermedias):
obtieneNuevaCeldaActual :: [Int] -> [Int] -> Int -> Int -> Int -> Int
obtieneNuevaCeldaActual celdasAnteriores celdasActuales posicionCeldaActual reg radio = 
  obtieneCeldaPresente celdasAnteriores celdasActuales reg radio posiciones
  where
    posiciones = [posicionCeldaActual-3,posicionCeldaActual-2,posicionCeldaActual-1,posicionCeldaActual,posicionCeldaActual+1,posicionCeldaActual+2,posicionCeldaActual+3]

obtienePrimerasCeldas :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
obtienePrimerasCeldas celdasAnteriores celdasActuales posicionCeldaActual reg radioVecindad
    | radioVecindad==1 = [primeraCelda]
    | radioVecindad==2 = [primeraCelda, segundaCelda]
    | radioVecindad==3 = [primeraCelda, segundaCelda, terceraCelda]
    | otherwise = error "El radio de vecindad no es valido o las listas estan vacias. En obtienePrimerasCeldas."
    where
      tam = length celdasActuales - 1
      posicionesPrim = [tam-3,tam-2,tam-1,posicionCeldaActual,posicionCeldaActual+1,posicionCeldaActual+2,posicionCeldaActual+3]
      posicionesSeg = [tam-2,tam-1,posicionCeldaActual,posicionCeldaActual+1,posicionCeldaActual+2,posicionCeldaActual+3,posicionCeldaActual+4]
      posicionesTerc = [tam-1,posicionCeldaActual,posicionCeldaActual+1,posicionCeldaActual+2,posicionCeldaActual+3,posicionCeldaActual+4,posicionCeldaActual+5]
      primeraCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesPrim
      segundaCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesSeg
      terceraCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesTerc
      
obtieneUltimasCeldas :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
obtieneUltimasCeldas celdasAnteriores celdasActuales posicionCeldaActual reg radioVecindad
    | radioVecindad==1 = [ultimaCelda]
    | radioVecindad==2 = [penultimaCelda, ultimaCelda]
    | radioVecindad==3 = [antepenultimaCelda, penultimaCelda, ultimaCelda]
    | otherwise = error "El radio de vecindad no es valido o las listas estan vacias. En obtieneUltimasCeldas."
    where
      posicionesUlt = [posicionCeldaActual-3,posicionCeldaActual-2,posicionCeldaActual-1,posicionCeldaActual,0,1,2]
      posicionesPen = [posicionCeldaActual-4,posicionCeldaActual-3,posicionCeldaActual-2,posicionCeldaActual-1,posicionCeldaActual,0,1]
      posicionesAnte = [posicionCeldaActual-5,posicionCeldaActual-4,posicionCeldaActual-3,posicionCeldaActual-2,posicionCeldaActual-1,posicionCeldaActual,0]
      ultimaCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesUlt
      penultimaCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesPen
      antepenultimaCelda = obtieneCeldaPresente celdasAnteriores celdasActuales reg radioVecindad posicionesAnte

obtieneCeldaPresente :: [Int] -> [Int] -> Int -> Int -> [Int] -> Int
obtieneCeldaPresente celdasAnteriores celdasActuales reg radio posiciones
    | radio == 1 = celdaFinalPresenteR1
    | radio == 2 = celdaFinalPresenteR2
    | radio == 3 = celdaFinalPresenteR3
    | otherwise = error "El radio de la vecindad no es valido o el valor anterior de la celda no es 0 o 1."
    where
      posMenos3 = primero posiciones
      posMenos2 = posiciones !! 1
      posMenos1 = posiciones !! 2
      pos = posiciones !! 3
      posMas1 = posiciones !! 4
      posMas2 = posiciones !! 5
      posMas3 = posiciones !! 6
      celdaIz3 = celdasActuales !! posMenos3
      celdaIz2 = celdasActuales !! posMenos2
      celdaIz1 = celdasActuales !! posMenos1
      celdaActual = celdasActuales !! pos
      celdaDer1 = celdasActuales !! posMas1
      celdaDer2 = celdasActuales !! posMas2
      celdaDer3 = celdasActuales !! posMas3
      celdaCentralPresenteR1 = regla reg celdaIz1 celdaActual celdaDer1
      celdaCentralPresenteR2 = reglaVecindad reg 2 0 celdaIz2 celdaIz1 celdaActual celdaDer1 celdaDer2 0
      celdaCentralPresenteR3 = reglaVecindad reg 3 celdaIz3 celdaIz2 celdaIz1 celdaActual celdaDer1 celdaDer2 celdaDer3
      celdaAnterior = celdasAnteriores !! pos
      esValorCeldaActualEnPasado1 = celdaAnterior == 1
      celdaFinalPresenteR1 | esValorCeldaActualEnPasado1 = abs $ celdaCentralPresenteR1 - celdaAnterior | otherwise = celdaCentralPresenteR1
      celdaFinalPresenteR2 | esValorCeldaActualEnPasado1 = abs $ celdaCentralPresenteR2 - celdaAnterior | otherwise = celdaCentralPresenteR2
      celdaFinalPresenteR3 | esValorCeldaActualEnPasado1 = abs $ celdaCentralPresenteR3 - celdaAnterior | otherwise = celdaCentralPresenteR3

-- Ejecuta el AC de segundo orden (se realizan varios pasos en la evolución):
-- ejecutaACSO :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int]
-- ejecutaACSO regla radio pasos ciclo = ejecuta regla radio pasos ciclo [ciclo]

-- ejecuta :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int] -> [CycleSO Int]
-- ejecuta regla radio pasos ciclo acum
--     | pasos==0 = acum
--     | otherwise = ejecuta regla radio (pasos-1) nuevoCiclo (acum ++ [nuevoCiclo])
--     where
--       anterior = pasado ciclo
--       actual = presente ciclo
--       actualizacion = aplicaReglaSegundoOrden regla radio anterior actual
--       nuevoCiclo = CycleSO {pasado=actual, presente=actualizacion}

ejecutaACSO :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO regla radio pasos ciclo
  | pasos==0 = []
  | otherwise = nuevoCiclo : ejecutaACSO regla radio (pasos-1) nuevoCiclo
  where 
    anterior = pasado ciclo
    actual = presente ciclo
    actualizacion = aplicaReglaSegundoOrden regla radio anterior actual
    nuevoCiclo = CycleSO {pasado=actual, presente=actualizacion}


-- Genera el AC de segundo orden:
generaACSO :: Int -> Int -> Int -> CycleSO Int -> [[Int]]
generaACSO regla radio pasos inicial = [pasado inicial, presente inicial] ++ presentes
  where
    presentes = vista' <$> ejecutaACSO regla radio pasos inicial

-- Muestra el AC de segundo orden:
muestraACSO :: Int -> Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO n regla radio pasos inicial = P.mapM_ putStrLn $ L.take n automata
    where
        automata = fmap muestra . vista' <$> ejecutaACSO regla radio pasos inicial
        muestra 0 = ' '
        muestra 1 = '*'




creaNumeroAleatorioMedianteAC :: IO Int
creaNumeroAleatorioMedianteAC = do
  gen <- newStdGen
  let (semilla, _) = randomR (30, 5000) gen :: (Int, StdGen)
  --semilla <- now
  let listaAleatorios = generaAleatoriosL semilla
      listaAleatoriosBase2 = L.concat (cambioABase2Lista listaAleatorios)            
      inicia = iniciaAC numCeldas listaAleatoriosBase2
      automata = generaAC numCeldas (regla 30) inicia
      indices = [1..genericLength automata-1]
      listaCentros = L.take 16 [elementoCentral f | f<-automata]
      num = deListaBinarioANum listaCentros
  return num

creaACSegundoOrdenRadio2 :: IO ()
creaACSegundoOrdenRadio2 = do
  gen <- newStdGen
  let (semilla1, nuevoGen) = randomR (30, 5000) gen :: (Int, StdGen)
  let (semilla2, _) = randomR (5000, 90000) gen :: (Int, StdGen)
  let a1 = L.take numCeldas $ L.concat (cambioABase2Lista (generaAleatoriosL semilla1))
      a2 = L.take numCeldas $ L.concat (cambioABase2Lista (generaAleatoriosL semilla2))
      inicio = inicializacion numCeldas a1 a2
      automata = L.take 5 $ generaACSO reglaAC 2 numPasos inicio
      tam = L.length automata
      confACReversible = inicializacion numCeldas (automata !! (tam-1)) (automata !! (tam-2))
      automataR = L.take 5 $ generaACSO reglaAC 2 numPasos confACReversible
  imprime "Autómata celular reversible de radio=2 (hacia adelante):"
  muestraACSO 50 reglaAC 2 numPasos inicio
  imprime "Autómata celular reversible de radio=2 (hacia atrás):"
  muestraACSO 50 reglaAC 2 numPasos confACReversible


creaACSegundoOrdenRadio3 :: IO ()
creaACSegundoOrdenRadio3 = do
  gen <- newStdGen
  let (semilla1, nuevoGen) = randomR (30, 5000) gen :: (Int, StdGen)
  let (semilla2, _) = randomR (5000, 90000) gen :: (Int, StdGen)
  let a1 = L.take numCeldas $ L.concat (cambioABase2Lista (generaAleatoriosL semilla1))
      a2 = L.take numCeldas $ L.concat (cambioABase2Lista (generaAleatoriosL semilla2))
      inicio = inicializacion numCeldas a1 a2
      automata = L.take 5 $ generaACSO reglaAC 3 numPasos inicio
      tam = L.length automata
      confACReversible = inicializacion numCeldas (automata !! (tam-1)) (automata !! (tam-2))
      automataR = L.take 5 $ generaACSO reglaAC 3 numPasos confACReversible
  imprime "Autómata celular reversible de radio=3 (hacia adelante):"
  muestraACSO 50 reglaAC 3 numPasos inicio
  imprime "Autómata celular reversible de radio=3 (hacia atrás):"
  muestraACSO 50 reglaAC 3 numPasos confACReversible