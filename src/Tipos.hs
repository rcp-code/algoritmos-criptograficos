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

--Tipos de datos para los autómatas celulares: Cycle y CycleSO

{- Tipo Cycle a:
        a: representa el tipo del Cycle (puede ser Int, Integer, Double, String, etc)
        El primer elemento del Cycle es un número entero (Int) que corresponde al tamaño
        Los siguientes corresponden a los dos primeros elementos del ciclo
        El último elemento es una lista infinita de elementos (su tipo es el que se establezca al crear los datos)
    Este tipo "hereda" de las clases Functor (es posible tratar este tipo como Comonad), Show (permite mostrar este tipo por pantalla) y Eq (compara si dos datos de tipo Cycle son iguales) -}
data Cycle a = Cycle Int a a (InfList a) 
                  deriving (Functor, Show, Eq)
                  
{- Tipo CycleSO a:
        a: representa el tipo del Cycle (puede ser Int, Integer, Double, String, etc)
        Es un registro en el que se almacenan los siguientes datos:
            - nCeldas: número de celdas
            - pasado: una lista de valores de las celdas del autómata en el instante t-1
            - presente: una lista de valores de las celdas del autómata en el instante t 
    Este tipo "hereda" de las clases Functor (es posible tratar este tipo como Comonad), Show (permite mostrar este tipo por pantalla) y Eq (compara si dos datos de tipo Cycle son iguales) -}
data CycleSO a = CycleSO {nCeldas :: Int
                , pasado :: [a]                 -- vecindad en el instante t-1
                , presente :: [a]               -- vecindad en el instante t
                } deriving (Functor, Show, Eq)  

{- Tipo ClavePublicaYPrivadaRSA:
        Registro que almacena los siguientes datos:
            e: exponente de cifrado
            n: parte de las claves pública y privada
            d: exponente de descifrado
            parPublico: clave pública
            parPrivado: clave privada 
    Este tipo "hereda" de las clases Show (permite mostrar este tipo por pantalla) y Eq (compara si dos datos de tipo Cycle son iguales) -}
data ClavePublicaYPrivadaRSA = ClavePublicaYPrivadaRSA {e :: Integer
                                                , n :: Integer
                                                , d :: Integer
                                                , parPublico :: Clave
                                                , parPrivado :: Clave
                                                } deriving (Show, Eq)

{- Tipo ClavePrivada:
        Registro que almacena tanto la clave completa como las reglas de los autómatas correspondientes (CAL, CAR, CAC y CAS) -}
data ClavePrivada = ClavePrivada {k :: [Int]                --clave completa
                            , kCAL :: [Int]                 --parte correspondiente al CAL
                            , kCAR :: [Int]                 --parte correspondiente al CAR
                            , kCAC :: [Int]                 --parte correspondiente al CAC
                            , kCAS :: [[Int]]               --parte correspondiente al CAS
                            } deriving (Show)      

--Redefinición del par de enteros, ahora llamado Clave
type Clave = (Integer, Integer)

--Redefinición de String, ahora llamado Mensaje
type Mensaje = String


