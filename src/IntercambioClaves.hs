module IntercambioClaves where


    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo 
                de intercambio de claves de Diffie-Hellman
    ----------------------------------------------------------------------}

import Util
import AC
import Tipos

    {----------------------------------------------------------------------
            Incluye las funciones necesarias para el algoritmo 
                de intercambio de claves de Diffie-Hellman
    ----------------------------------------------------------------------}

generador :: Int -> Integer -> Tupla Integer
generador semilla p = if gPos `elem` gmps then Tupla 2 (pSegPos, gPos) else error $ "No se ha encontrado un grupo multiplicativo para " ++ show p
    where
        pSeg = primoSeguro p
        g = generaAleatorio semilla 2 (fromInteger pSeg-2)
        pSegPos = toInteger $ abs pSeg
        gPos = toInteger $ abs g
        gmps = obtieneGrupoMultiplicativo p
        --gEnGP = gPos `elem` gmps

-- generador :: Int -> Int -> Int -> Tupla
-- generador semilla1 semilla2 p
--     | null gmps = error $ "No se ha encontrado un grupo multiplicativo para " ++ show p
--     | otherwise = (primoSeg, g)
--     where 
--         gmps = obtieneGrupoMultiplicativo p
--         tam = tamLista gmps - 1
--         indice = generaAleatorio semilla1 1 tam
--         primo = gmps !! indice
--         primoSeg = primoSeguro primo
--         g = generaAleatorio semilla2 2 (primoSeg-2)

eligeSecreto :: Int -> Tupla Integer -> Integer
eligeSecreto semilla par@(Tupla 2 (p,g)) = mod gex p
    where
        x = abs $ generaAleatorio semilla 1 (fromInteger (p-2))
        gex = g^x

-- calculaClaveCompartida :: Integer -> Integer -> Integer -> Integer -> Integer
-- calculaClaveCompartida p g x y = mod gyx p
--     where
--         gy = g^y
--         gyx = gy^x

calculaClaveCompartida :: Tupla Integer -> Integer -> Integer -> Integer
calculaClaveCompartida gen@(Tupla 2 (p,g)) x y = mod gyx p 
    where 
        gy = g^y 
        gyx = gy^x

compruebaClaveCompartida :: Integer -> Integer -> Bool
compruebaClaveCompartida clave1 clave2 = clave1==clave2
