{-# OPTIONS_GHC -Wno-missing-fields #-}
module AutomataCelular where

    {----------------------------------------------------------------------
                  Incluye las funciones necesarias para 
                      crear los autómatas celulares.
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


    {-------------------------------------------------------------------------
                      Autómatas celulares elementales
    -------------------------------------------------------------------------}
instance Comonad Cycle where
  extract (Cycle _ _ cen _) = cen
  duplicate cel@(Cycle n _ _ _) = deListaACiclo $ Inf.take n $ Inf.iterate cambia cel
    where
      cambia (Cycle n _ cen (der:::ds)) = Cycle n cen der ds

    {-------------------------------------------------------------------------
                      Tratamiento de autómatas celulares
    -------------------------------------------------------------------------}

--Transforma un ciclo en una lista 
deCicloALista :: Cycle a -> [a]
deCicloALista (Cycle n _ cen der) = Inf.take n (cen:::der)

--Transforma una lista en un ciclo
deListaACiclo :: [a] -> Cycle a
deListaACiclo []  = let a = a in Cycle 0 a a (Inf.repeat a)
deListaACiclo lista = let cel:::der = Inf.cycle lista in Cycle (P.length lista) (P.last lista) cel der

    {-------------------------------------------------------------------------
                      Creación de los autómatas celulares
    -------------------------------------------------------------------------}

--Cálculo de una nueva celda a partir de una regla y un vecindario (celdas izquierda, central y derecha)
regla :: (Integral a, Integral b) => a -> b -> b -> b -> a
regla r izq cen der = r `div` (2^(4*izq + 2*cen + der)) `mod` 2

--Aplica la regla a un vecindario para obtener una nueva celda
ejecutaRegla :: (a1 -> a1 -> a1 -> a2) -> Cycle a1 -> a2
ejecutaRegla regla (Cycle _ izq cen (der:::_)) = regla izq cen der

-- Aplica la regla a todas las celdas del autómata
ejecutaAC :: (Eq a, Integral a) => (a -> a -> a -> a) -> Cycle a -> InfList (Cycle a)
ejecutaAC regla = Inf.iterate (=>> ejecutaRegla regla)    -- => extiende con los argumentos intercambiados

    {-----------------------------------------------------------------------------------
                              Inicialización del AC
    ------------------------------------------------------------------------------------}

--Inicializa el AC con la configuración inicial (puede ser aleatoria o no)
iniciaAC :: (Integral a, Num a) => Int -> [a] -> Cycle a
iniciaAC numCels lista = deListaACiclo $ centro $ incluyeVecinosDer numCels lista
  where
    incluyeVecinosDer numCels lista = P.take numCels $ lista P.++ P.repeat 0
    centro = Inf.take numCels . Inf.drop (numCels `div` 2+1) . Inf.cycle

    {-----------------------------------------------------------------------------------
                                  Generación del AC
    ------------------------------------------------------------------------------------}

--Genera el autómata a partir de una regla y una configuración inicial, con n celdas
generaAC :: (Integral a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> [[a]]
generaAC n regla inicial = Inf.take n automata
  where
    automata = deCicloALista <$> ejecutaAC regla inicial

    {-----------------------------------------------------------------------------------
                                    Muestra el AC
    ------------------------------------------------------------------------------------}

--Muestra el autómata
muestraAC :: (Eq a, Integral a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> IO ()
muestraAC n regla ini = P.mapM_ putStrLn $ Inf.take n automata
  where
    automata = fmap muestra . deCicloALista <$> ejecutaAC regla ini   -- <$> - sinónimo infijo de fmap, que reemplaza todas las ubicaciones en la entrada por el mismo valor
    muestra 0 = ' '
    muestra 1 = '*'


    {-----------------------------------------------------------------------------------
                            Autómatas de segundo orden
    ------------------------------------------------------------------------------------}

deCicloALista' :: CycleSO Int -> [Int]
deCicloALista' (CycleSO nc pasado presente) = presente

--Configuraciones iniciales para el autómata de segundo orden
inicializacion :: Int -> [Int] -> [Int] -> CycleSO Int
inicializacion n lista1 lista2 = CycleSO {nCeldas=n, pasado=lista1, presente=lista2}

--Cálculo de una nueva celda a partir de una regla y un vecindario (depende del radio de vecindad)
reglaVecindad :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
reglaVecindad reg radio izq3 izq2 izq1 cen der1 der2 der3
  | radio==2 = (reg `div` divisorR2) `mod` 2
  | radio==3 = fromInteger (toInteger reg `div` divisorR3) `mod` 2
  | otherwise = error "El radio de vecindad no es valido."
  where
    divisorR2 = 2^(16*izq2 + 8*izq1 + 4*cen + 2*der1 + der2)
    divisorR3 = 2^(64*izq3 + 32*izq2 + 16*izq1 + 8*cen + 4*der1 + 2*der2 + der3)

--Aplica la regla a un conjunto de celdas para obtener nuevas celdas
aplicaReglaSegundoOrden :: Int -> Int -> [Int] -> [Int] -> [Int]
aplicaReglaSegundoOrden reg radioVecindad pasado presente
    | radioVecindad == 1 || radioVecindad == 2 || radioVecindad == 3 = primerasCeldas ++ celdasIntermedias ++ ultimasCeldas
    | otherwise = error "El radio de vecindad no es valido."
    where
      tam = length presente
      primerasCeldas = obtienePrimerasCeldas pasado presente 0 reg radioVecindad
      celdasIntermedias = [obtieneNuevaCeldaActual pasado presente i reg radioVecindad | i <- [radioVecindad..(tam-radioVecindad-1)]]
      ultimasCeldas = obtieneUltimasCeldas pasado presente (tam-1) reg radioVecindad

--Obtiene las nuevas celdas a partir de la regla (celdas intermedias):
obtieneNuevaCeldaActual :: [Int] -> [Int] -> Int -> Int -> Int -> Int
obtieneNuevaCeldaActual celdasAnteriores celdasActuales posicionCeldaActual reg radio =
  obtieneCeldaPresente celdasAnteriores celdasActuales reg radio posiciones
  where
    posiciones = [posicionCeldaActual-3,posicionCeldaActual-2,posicionCeldaActual-1,posicionCeldaActual,posicionCeldaActual+1,posicionCeldaActual+2,posicionCeldaActual+3]

--Obtiene las primeras celdas (las de la frontera)
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

--Obtiene las últimas celdas (las de la frontera)
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

--Obtiene nueva celda en el instante t
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

--Aplica la regla de evolución a todo el autómata
ejecutaACSO :: Int -> Int -> Int -> CycleSO Int -> [CycleSO Int]
ejecutaACSO regla radio pasos ciclo
  | pasos==0 = []
  | otherwise = nuevoCiclo : ejecutaACSO regla radio (pasos-1) nuevoCiclo
  where
    anterior = pasado ciclo
    actual = presente ciclo
    actualizacion = aplicaReglaSegundoOrden regla radio anterior actual
    nuevoCiclo = CycleSO {pasado=actual, presente=actualizacion}

--Genera el AC de segundo orden:
generaACSO :: Int -> Int -> Int -> CycleSO Int -> [[Int]]
generaACSO regla radio pasos inicial = [pasado inicial, presente inicial] ++ presentes
  where
    presentes = deCicloALista' <$> ejecutaACSO regla radio pasos inicial

--Muestra el AC de segundo orden:
muestraACSO :: Int -> Int -> Int -> Int -> CycleSO Int -> IO ()
muestraACSO n regla radio pasos inicial = P.mapM_ putStrLn $ L.take n automata
    where
        automata = fmap muestra . deCicloALista' <$> ejecutaACSO regla radio pasos inicial
        muestra 0 = ' '
        muestra 1 = '*'



    {-------------------------------------------------------------------------
                      Creación de números aleatorios
                          y AC de segundo orden
    -------------------------------------------------------------------------}

--Crea y muestra un AC elemental con condiciones iniciales aleatorias
creaYmuestraACelementalCondicionesAleatorias :: IO ()
creaYmuestraACelementalCondicionesAleatorias = do
  imprime "Introduce el número de celdas para el autómata celular: "
  celdas <- getLine
  imprime "Introduce la regla para el autómata celular: "
  reg <- getLine
  let numCeldas = read celdas :: Int
  let r = read reg :: Int
  gen <- newStdGen
  let (semilla, nuevoGen) = randomR (1, 1000) gen :: (Int, StdGen)
  let listaAleatorios = generaAleatoriosL semilla
  let configuracionInicial = L.concat (cambioABase2Lista listaAleatorios)
  let inicia = iniciaAC numCeldas configuracionInicial
  muestraAC numCeldas (regla r) inicia

--Crea y muestra un AC elemental con condiciones iniciales fijas (una sola celda activa en el centro)
creaYmuestraACelementalCondicionesFijas :: IO ()
creaYmuestraACelementalCondicionesFijas = do
  imprime "Introduce el número de celdas para el autómata celular: "
  celdas <- getLine
  imprime "Introduce la regla para el autómata celular: "
  reg <- getLine
  let numCeldas = read celdas :: Int
  let r = read reg :: Int
  let configuracionInicial = [1]
  let inicia = iniciaAC numCeldas configuracionInicial
  muestraAC numCeldas (regla r) inicia

--Obtiene un número primo aleatorio a partir de un autómata celular elemental
obtienePrimoAleatorio :: IO Integer
obtienePrimoAleatorio = do
    gen <- newStdGen
    let (semillaLista, nuevoGen) = randomR (1, 1000) gen :: (Int, StdGen)
    let (semillaCeldas, genNuevo) = randomR (100, 6005) nuevoGen :: (Int, StdGen)
    let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        --genera número aleatorio de celdas que tendrá el autómata
    let numCeldas | even numCeldasAlt = numCeldasAlt +1                         --si el número de celdas que se genera de manera aleatoria es par, le suma uno para que sea impar
                  | otherwise = numCeldasAlt
    let listaAleatorios = generaAleatoriosL semillaLista
    let listaAleatoriosBase2 = L.concat (cambioABase2Lista listaAleatorios)                --se pasa la lista de aleatorios a base 2 y se aplana
    --Se crea el autómata para generar el número pseudoaleatorio
    let inicia = iniciaAC numCeldas listaAleatoriosBase2
    let automata = generaAC numCeldas (regla 30) inicia
    let indices = [1..genericLength automata-1]
    let listaCentros = L.take 17 [elementoCentral f | f<-automata]
    let num = deListaBinarioANum listaCentros
    let control = esPrimo (toInteger num)
    semillaPrimos <- now
    let opcion = generaAleatorio' semillaPrimos             --obtiene un número aleatorio entre 0 y 1 para elegir de manera aleatoria si se buscará un primo por encima o por debajo del número
    --se genera el número primo a partir del AC
    let p | control = num                                   --si el número ya es primo, no hay que buscarlo
          | opcion == 0 = obtienePrimoCercanoInf num
          | otherwise = obtienePrimoCercanoSup num
    return (toInteger p)

--Obtiene un número aleatorio mediante un autómata celular elemental
obtieneNumeroAleatorioMedianteAC :: IO Int
obtieneNumeroAleatorioMedianteAC = do
  gen <- newStdGen
  let (semilla, _) = randomR (30, 5000) gen :: (Int, StdGen)
  let listaAleatorios = generaAleatoriosL semilla
      listaAleatoriosBase2 = L.concat (cambioABase2Lista listaAleatorios)
      inicia = iniciaAC numCeldas listaAleatoriosBase2
      automata = generaAC numCeldas (regla 30) inicia
      indices = [1..genericLength automata-1]
      listaCentros = L.take 16 [elementoCentral f | f<-automata]
      num = deListaBinarioANum listaCentros
  return num

--Crea y muestra un autómata de segundo orden de radio 2
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
  if automata == reverse automataR then do
    imprime "El autómata reversible es igual que el original si le damos la vuelta."
  else do
    imprime "El autómata reversible no se ha construido correctamente."

--Crea y muestra un autómata de segundo orden de radio 3
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
  if automata == reverse automataR then do
    imprime "El autómata reversible es igual que el original si le damos la vuelta."
  else do
    imprime "El autómata reversible no se ha construido correctamente."