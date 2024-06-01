module TestsPropiedades where

    {----------------------------------------------------------------------
                    Comprobaciones de propiedades   
    ----------------------------------------------------------------------}

import Test.QuickCheck
import AutomataCelular
import CifradoBloqueIO
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

prop_codifica_descodifica' :: Mensaje -> Bool
prop_codifica_descodifica' m = m == descodifica (codificaEnBinario m)

prop_codifica_descodifica :: Mensaje -> Property
prop_codifica_descodifica (c:m) = m /= [] && m/="" && comprobarCaracteresAdmitidos m && (valorNumerico == 32 || valorNumerico == 33 || valorNumerico == 44 || valorNumerico == 46 ||
                                    (valorNumerico >= 48 && valorNumerico <= 57) || (valorNumerico >= 65 && valorNumerico <= 90) ||
                                    (valorNumerico >= 97 && valorNumerico <= 122))
                                    ==> m == descodifica (codificaEnBinario m)
    where
        valorNumerico = ord c

comprobarCaracteresAdmitidos :: Mensaje -> Bool
comprobarCaracteresAdmitidos = all isPrint

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
                    Comprobaciones relativas a asociaciones
                        y transformaciones de datos
    ----------------------------------------------------------------------}

prop_int2bin :: Int -> Bool
prop_int2bin x = bin2int (int2bin y) == y
    where
        y = abs x

prop_int2bin64 :: Int -> Bool
prop_int2bin64 x = bin2int64 (int2bin64 y) == y
    where
        y = abs x

prop_strToInteger :: String -> Property
prop_strToInteger m = m /= [] ==> strToInteger1 m == strToInteger m

prop_integerToStr :: Integer -> Property
prop_integerToStr n = n>0 ==> integerToStr1 n == integerToStr n

prop_strToInteger_integerToStr :: String -> Property
prop_strToInteger_integerToStr m = m /= [] && 0 /= (ord . head) m ==> m == (integerToStr . strToInteger) m

prop_integerToStr_strToInteger :: Integer -> Property
prop_integerToStr_strToInteger n = n > 0 ==> n == (strToInteger . integerToStr) n

    {----------------------------------------------------------------------
                Comprobaciones relativas a funciones de RSA
    ----------------------------------------------------------------------}

prop_clavePrivada :: Integer -> Integer -> Integer -> Property
prop_clavePrivada e d phiN = e>=2 && d>=2 && phiN>=2 && esPrimo e && esPrimo d && e>=100 && d>=100 ==> d*e == mod 1 phiN

prop_exponenciacionModular :: Integer -> Integer -> Integer -> Property
prop_exponenciacionModular c e n = e>0 && n>0 && esPrimo e ==> exponenciacionModular c e n == mod exp n
    where
        exp = c^e

prop_cifradoRSA :: Mensaje -> Clave -> Property
prop_cifradoRSA m = undefined

prop_descifradoRSA :: Mensaje -> Clave -> Property
prop_descifradoRSA m = undefined

prop_cifradoRSA_descifradoRSA :: Mensaje -> Clave -> Clave -> Property
prop_cifradoRSA_descifradoRSA (c:m) clavePublica clavePrivada = not (null m) && comprobarCaracteresAdmitidos m && (valorNumerico == 32 || valorNumerico == 33 || valorNumerico == 44 || valorNumerico == 46 ||
                                    (valorNumerico >= 48 && valorNumerico <= 57) || (valorNumerico >= 65 && valorNumerico <= 90) ||
                                    (valorNumerico >= 97 && valorNumerico <= 122)) && n>0 && esPrimo e && esPrimo d && e/=d && d>=100 && e>=100
                                            ==> m == descifradoRSA (cifradoRSA m clavePublica) clavePrivada
                                            where
                                                n = fst clavePublica
                                                e = snd clavePublica
                                                d = snd clavePrivada
                                                valorNumerico = ord c

    {----------------------------------------------------------------------
                Comprobaciones relativas a cifrado de bloques 
                        basado en AC de segundo orden
    ----------------------------------------------------------------------}

prop_cifradoSimplificado_descifradoSimplificado' :: [Int] -> [Int] -> Int -> Int -> Int -> Bool
prop_cifradoSimplificado_descifradoSimplificado' clave texto regla radio pasos = texto == primero datosDescifrados
    where
        datosCifrados = versionSimplificadaCifrado clave texto regla radio pasos
        datosDescifrados = versionSimplificadaDescifrado clave (reverse datosCifrados) regla radio pasos


prop_cifradoSimplificado_descifradoSimplificado :: [Int] -> [Int] -> Int -> Int -> Int -> Property
prop_cifradoSimplificado_descifradoSimplificado clave texto regla radio pasos = texto /= [] && pasos>0 && clave /= [] && regla>0 && (radio>=1 && radio<=3) &&
                                length clave >= 3 && length texto >=3 && all compruebaElemento clave && all compruebaElemento texto
                                ==> texto == primero datosDescifrados
    where
        datosCifrados = versionSimplificadaCifrado clave texto regla radio pasos
        datosDescifrados = versionSimplificadaDescifrado clave (reverse datosCifrados) regla radio pasos
        compruebaElemento x = x==0 || x==1
        

prop_cifrado_descifrado_conAutomatasDeSegundoOrden :: Mensaje -> Property
prop_cifrado_descifrado_conAutomatasDeSegundoOrden texto = condiciones ==> texto == cambiaListasEnterosATexto textoDescifrado
    where 
        filtrado = filter estaEnCaracteres texto
        condiciones = texto /= [] && (length filtrado == length texto)
        textoCod = preparaTexto texto
        datosCifrado@(textoCifrado, residuo, nuevoCAS, clave) = unsafePerformIO $ cifrado textoCod
        (textoDescifrado, _, _) = unsafePerformIO $ descifrado clave nuevoCAS textoCifrado residuo