{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Tipos where

    {----------------------------------------------------------------------
            Fichero en el que se incluyen tipos nuevos o redefinidos
    ----------------------------------------------------------------------}

import Data.InfList as Inf
import Data.Functor
import Control.Comonad
import Data.Vector

    {----------------------------------------------------------------------
                                Generales
    ----------------------------------------------------------------------}

data Tripleta a = Tripleta (a, a, a) deriving (Show, Eq)

type Pos = (Int, Int)

    {----------------------------------------------------------------------
                Relacionado con Autómatas Celulares elementales
    ----------------------------------------------------------------------}

data Cycle a = Cycle Int a a (InfList a) 
                  deriving (Functor, Show)

data CycleSO a = CycleSO {nCeldas :: Int
                , pasado :: [a]                 -- vecindad en el instante t-1
                , presente :: [a]               -- vecindad en el instante t
                } deriving (Show, Functor)  

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


