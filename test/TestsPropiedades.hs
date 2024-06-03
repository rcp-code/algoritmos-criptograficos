module TestsPropiedades where

    {----------------------------------------------------------------------
                    Comprobaciones de propiedades   
    ----------------------------------------------------------------------}

import Test.QuickCheck
import AutomataCelular
import CifradoBloque
import CifradoWolfram
import Constantes
import RSA
import Tipos
import UtilCripto
import UtilGeneral
import Data.Char
import GHC.IO (unsafePerformIO)



    {----------------------------------------------------------------------
                        Comprobaciones generales
    ----------------------------------------------------------------------}

prop_entero_binario :: Int -> Property
prop_entero_binario n = n>=0 ==> n == deListaBinarioANum binario
    where
        binario = cambioABase2 n

prop_numeros_digitos :: Int -> Property
prop_numeros_digitos n = n>=0 ==> n == deDigitosANum ds
    where
        ds = digitos n

prop_codifica_descodifica :: Mensaje -> Property
prop_codifica_descodifica (c:m) = m /= [] && m/="" && m/=" " && not (null m) && estaEnCaracteres c
                                    ==> m == descodifica (codificaEnBinario m)
    where
        valorNumerico = transformaCaracterEnInt c

    {----------------------------------------------------------------------
                Comprobaciones relativas a AC de segundo orden
    ----------------------------------------------------------------------}

prop_automata_automataR :: [[Int]] -> [[Int]] -> Bool
prop_automata_automataR automata automataR = automata == reverse automataR

prop_automata_automataR'' :: [[Int]] -> [[Int]] -> Property
prop_automata_automataR'' automata automataR = automata /= [] && automataR /= [] && todosElementosBinarios automata && todosElementosBinarios automataR
                ==> automata == reverse automataR

todosElementosBinarios :: [[Int]] -> Bool
todosElementosBinarios lista = all compruebaElemento listaConcatenada
    where
        listaConcatenada = concat lista
        compruebaElemento x = x==0 || x==1

prop_automata_automataR' :: Int -> (Int -> Int -> Int -> Int) -> [[Int]] -> Int -> Property
prop_automata_automataR' numeroCeldas regla configuracionesIniciales radioVecindad = configuracionesIniciales /= [] &&
            numeroCeldas>100 && pasado inicio /= [] && presente inicio /= [] && length (presente inicio) >= 3 && length (pasado inicio) >= 3 &&
                length configuracionesIniciales == 2 && (radioVecindad >= 1 && radioVecindad <= 3) && todosElementosBinarios automata && todosElementosBinarios automataR
                ==> automata == reverse automataR
    where
        inicio = inicializacion (length configuracionesIniciales - 1) (primero configuracionesIniciales) (ultimo configuracionesIniciales)
        automata = generaACSO reglaAC radioVecindad numPasos inicio
        inicioR = inicializacion (length configuracionesIniciales - 1) (ultimo automata) (primero automata)
        automataR = generaACSO reglaAC radioVecindad numPasos inicioR

    {----------------------------------------------------------------------
                Comprobaciones relativas a funciones de RSA
    ----------------------------------------------------------------------}

prop_clavePrivada :: Integer -> Integer -> Property
prop_clavePrivada p q = e>=2 && d>=2 && phiN>=2 && esPrimo p && esPrimo q && esPrimo e && esPrimo d ==> d*e == mod 1 phiN
    where
        phiN = calculoPhi p q
        clavePrivYPub = clavesPublicaYPrivada (p,q)
        clavePublica = parPublico clavePrivYPub
        clavePrivada = parPrivado clavePrivYPub
        e = snd clavePublica
        n = fst clavePublica
        d = snd clavePrivada

prop_exponenciacionModular :: Integer -> Integer -> Integer -> Property
prop_exponenciacionModular c e n = e>0 && n>0 && esPrimo e ==> exponenciacionModular c e n == mod exp n
    where
        exp = c^e

prop_cifradoRSA_descifradoRSA :: Mensaje -> Property
prop_cifradoRSA_descifradoRSA (c:m) = not (null m) && m/="" && esPrimo e && esPrimo d && e/=d && d>=100 && e>=100 ==> m == textoDescifrado
    where
        p = unsafePerformIO obtienePrimoAleatorio
        q = unsafePerformIO obtienePrimoAleatorio
        clavePrivYPub = clavesPublicaYPrivada (p,q)
        clavePublica = parPublico clavePrivYPub
        clavePrivada = parPrivado clavePrivYPub
        e = snd clavePublica
        n = fst clavePublica
        d = snd clavePrivada
        textoCifrado = cifradoRSA m clavePublica
        textoDescifrado = descifradoRSA textoCifrado clavePrivada

    {----------------------------------------------------------------------
                Comprobaciones relativas a cifrado de bloques 
                        basado en AC de segundo orden
    ----------------------------------------------------------------------}

prop_cifrado_descifrado_conAutomatasDeSegundoOrden :: Mensaje -> Property
prop_cifrado_descifrado_conAutomatasDeSegundoOrden texto = condiciones ==> texto == cambiaListasEnterosATexto textoDescifrado
    where
        filtrado = filter estaEnCaracteres texto
        condiciones = texto /= [] && (length filtrado == length texto)
        textoCod = preparaTexto texto
        datosCifrado@(textoCifrado, residuo, nuevoCAS, clave) = cifrado textoCod
        (textoDescifrado, _, _) = descifrado clave nuevoCAS textoCifrado residuo