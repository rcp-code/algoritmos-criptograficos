{-# LANGUAGE DeriveFunctor #-}
module Tipos where


import Data.InfList as Inf
import Data.Functor

    {----------------------------------------------------------------------
                Fichero en el que se incluyen los tipos
    ----------------------------------------------------------------------}

{- Tipo de dato Cycle: 
    - Lo primero es tomar un número entero que será el que determine el tamaño del ciclo. 
    - Después un elemento del ciclo, que será el correspondiente al de la izquierda, 
        y otro elemento que será el central. Después una lista infinita de elementos a la derecha. -}
data Cycle a = Cycle Int a a (InfList a) 
                  deriving (Functor, Show)

data Tupla a = Tupla Int (a, a) deriving (Show, Eq)

data Tripleta a = Tripleta Int (a, a, a) deriving (Show, Eq)

data ClavePublicaYPrivada = ClavePublicaYPrivada {e :: Integer
                                                , n :: Integer
                                                , d :: Integer
                                                , publica :: Clave
                                                , privada :: Clave
                                                } deriving (Eq, Show)

type Clave = (Integer, Integer)
type Mensaje = String


