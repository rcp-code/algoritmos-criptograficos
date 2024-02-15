{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Tipos where

    {----------------------------------------------------------------------
            Fichero en el que se incluyen tipos nuevos o redefinidos
    ----------------------------------------------------------------------}

import Data.InfList as Inf
import Data.Functor

    {----------------------------------------------------------------------
                                Generales
    ----------------------------------------------------------------------}

data Tripleta a = Tripleta (a, a, a) deriving (Show, Eq)

type Pos = (Int, Int)

    {----------------------------------------------------------------------
                Relacionado con Autómatas Celulares elementales
    ----------------------------------------------------------------------}

{- Tipo de dato Cycle: 
    - Lo primero es tomar un número entero que será el que determine el tamaño del ciclo. 
    - Después un elemento del ciclo, que será el correspondiente al de la izquierda, 
        y otro elemento que será el central. Después una lista infinita de elementos a la derecha. -}

data Cycle a = Cycle Int a a (InfList a) 
                  deriving (Functor, Show)

    {----------------------------------------------------------------------
                            Relacionado con RSA
    ----------------------------------------------------------------------}

data ClavePublicaYPrivadaRSA = ClavePublicaYPrivadaRSA {e :: Integer
                                                , n :: Integer
                                                , d :: Integer
                                                , parPublico :: Clave
                                                , parPrivado :: Clave
                                                } deriving (Show, Eq)

type Clave = (Integer, Integer)
type Mensaje = String


