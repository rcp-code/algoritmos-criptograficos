{-# LANGUAGE FlexibleInstances #-}
module AC where

    {----------------------------------------------------------------------
                  Incluye las funciones necesarias para 
                      los autómatas celulaautomata.
    ----------------------------------------------------------------------}

-- Módulos necesarios para crear las funciones del módulo actual
import Tipos
import Util
import Data.Functor
--import Control.Applicative
import Prelude as P
import Control.Comonad
import System.Random
import Data.InfList as Inf


--Inicialización de constantes
minCeldas :: Int
minCeldas = 1000

maxCeldas :: Int
maxCeldas = 5000

    {-------------------------------------------------------------------------
            Necesario para el tratamiento de los autómatas celulares
    -------------------------------------------------------------------------}

instance Comonad Cycle where
  extract (Cycle _ _ cel _) = cel
  duplicate cel@(Cycle n _ _ _) = deListaACiclo $ Inf.take n $ Inf.iterate cambia cel
    where 
      cambia (Cycle n _ cel (der:::ds)) = Cycle n cel der ds

instance RandomGen (Cycle Int) where
  next cel = let c = cel =>> ejecutaRegla (regla 30) in (deListaBinarioANum (deCicloALista c), c)     --la regla 30 es usada aquí para elegir una nueva célula de manera aleatoria
  split = (,) <*> (deListaACiclo . P.reverse . deCicloALista)

--Transforma un ciclo en una lista 
deCicloALista :: Cycle a -> [a]
deCicloALista (Cycle n _ cen der) = Inf.take n (cen:::der)

--Transforma una lista en un ciclo
deListaACiclo :: [a] -> Cycle a
deListaACiclo []  = let a = a in Cycle 0 a a (Inf.repeat a)      --asegura que nunca se accederá a un ciclo vacío
deListaACiclo lista = let cel:::der = Inf.cycle lista in Cycle (P.length lista) (P.last lista) cel der

    {-------------------------------------------------------------------------
                      Creación de los autómatas celulares
    -------------------------------------------------------------------------}

regla :: (Integral a, Integral b) => a -> b -> b -> b -> a
regla r izq cen der = r `div` (2^(4*izq + 2*cen + der)) `mod` 2

ejecutaRegla :: (a1 -> a1 -> a1 -> a2) -> Cycle a1 -> a2
ejecutaRegla regla (Cycle _ izq cel (der:::_)) = regla izq cel der

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
    automata = deCicloALista <$> ejecutaAC regla ini

    {-----------------------------------------------------------------------------------
                                    Muestra el AC
    ------------------------------------------------------------------------------------}

-- Solo para fines de comprobación para saber si el AC funciona bien
muestraAC :: (Eq a, Integral a, Num a) => Int -> (a -> a -> a -> a) -> Cycle a -> IO ()
muestraAC n regla ini = P.mapM_ putStrLn $ Inf.take n automata
  where 
    automata = fmap muestra . deCicloALista <$> ejecutaAC regla ini   -- <$> - sinónimo infijo de fmap, que reemplaza todas las ubicaciones en la entrada por el mismo valor
    muestra 0 = ' '
    muestra 1 = '*'

    {-----------------------------------------------------------------------------------
                                        Orden 2
    ------------------------------------------------------------------------------------}

