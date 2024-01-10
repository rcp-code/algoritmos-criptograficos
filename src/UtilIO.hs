module UtilIO where

    {----------------------------------------------------------------------
            Incluye algunas funciones IO auxiliares o de utilidad
              para distintos ficheros, de forma que queden todos
                      unificados en un mismo lugar.
    ----------------------------------------------------------------------}

import Data.Vector as V
import Prelude as P
import Data.Char
import Data.List as L
import System.Random
import Control.Monad as M
import Data.Functor
import Data.Time.Clock

    {----------------------------------------------------------------------
                            Funciones útiles IO
    ----------------------------------------------------------------------}

imprime :: String -> IO ()
imprime = putStrLn

leeMensaje :: IO String
leeMensaje = getLine

leeChar :: IO Char
leeChar = getChar

now :: IO Int
now = getCurrentTime Data.Functor.<&> (floor . fromRational . toRational . utctDayTime)