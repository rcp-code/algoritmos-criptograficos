module CifradoBloque where


import Tipos
import AutomataCelular as AC
import UtilGeneral
import UtilCripto
---
import Data.Bits
import Data.Char
import Data.List
import Constantes

    {----------------------------------------------------------------------
                Cifrado de bloque basado en AC de segundo orden
    ----------------------------------------------------------------------}

datosInicialesAleatorios :: Int -> [Int]
datosInicialesAleatorios semilla = concat (cambioABase2Lista (generaAleatoriosL semilla))

-- XOR aplicado a dos listas, número a número (bit a bit)
xorl :: [Int] -> [Int] -> [Int]
xorl datos1 datos2 = [xor2 x y | (x,y)<-zip datos1 datos2]
    where
        xor2 a b
            | xor' x y = 1
            | otherwise = 0
            where
                x = a==1
                y = b==1

divideEnDosSubBloques :: [Int] -> [[Int]]
divideEnDosSubBloques [] = error "El bloque de datos esta vacio, no se puede realizar la particion."
divideEnDosSubBloques bloque = [primeraSublista, segundaSublista]
    where
        tam = tamLista bloque - 1
        tamParticion = div tam 2
        primeraSublista = slicing bloque 0 tamParticion
        segundaSublista = slicing bloque (tamParticion + 1) tam

reglaReversible :: Int -> Int -> Int
reglaReversible r1 radioVecindad = 2^(2*radioVecindad+1) - r1 - 1

    {----------------------------------------------------------------------
                            Versión simplificada
    ----------------------------------------------------------------------}

inicializaACSegundoOrden :: Int -> [Int] -> [Int] -> CycleSO Int
inicializaACSegundoOrden n datos1 datos2 = CycleSO {nCeldas=n, pasado=take n datos1, presente=datos2}

versionSimplificadaCifrado :: Int -> CycleSO Int -> Int -> Int -> Int -> [[Int]]
versionSimplificadaCifrado clave inicial regla radio pasos = [datosCifrados, datosRes]
    where
        automata = generaACSO regla radio pasos inicial
        tamAutomata = tamLista automata
        datosResiduales = ultimo automata
        datosCifrados = obtieneSubLista automata (tamAutomata - 2)
        claveFormaLista = digitos clave
        datosRes = xorl claveFormaLista datosResiduales

versionSimplificadaDescifrado :: Int -> [[Int]] -> Int -> Int -> Int -> [[Int]]
versionSimplificadaDescifrado clave datos regla radio pasos = [datosInicialesAleatorios, descifrado]
    where
        claveFormaLista = digitos clave
        datosResiduales = ultimo datos
        datosCifrados = primero datos
        datosRes = xorl claveFormaLista datosResiduales
        inicial = inicializaACSegundoOrden (tamLista datosResiduales) datosRes datosCifrados
        reglaRev = reglaReversible regla radio                                                      -- radio de vecindad: 1
        automata = generaACSO reglaRev radio pasos inicial
        tamAutomata = tamLista automata
        datosInicialesAleatorios = ultimo automata
        descifrado = obtieneSubLista automata (tamAutomata - 2)


    {----------------------------------------------------------------------
                        Versión más segura de Bouvry 
    ----------------------------------------------------------------------}

    {----------------------------------------------------------------------
                                    Cifrado
    ----------------------------------------------------------------------}


-- Desplaza bits n veces a derecha o izquierda
desplazaBits :: [Int] -> Int -> Char -> [Int]
desplazaBits qn n desplazamiento
    | desplazamiento=='L' = drop n qn ++ take n qn   --desplaza bits a la izquierda
    | desplazamiento=='R' = drop m qn ++ take m qn   --desplaza bits a la derecha
    | otherwise = error "El desplazamiento no es correcto."
    where
        m = length qn - n

cas :: Int -> Int -> [Int] -> [Int] -> (Int, [[Int]])
cas regla pasos datos1 datos2 = (numeroDespl, nuevosDatosdatosInicialesAleatorios)
    where
        inicial = inicializacion (numBits `div` 4) datos1 datos2
        automata = generaACSO regla 2 pasos inicial
        p = div pasos 2                                        -- posición: pasos/2
        sublista1 = slicing automata 2 p                       -- sublista desde la posición 2 hasta p
        sublista2 = slicing automata (p+1) pasos               -- sublista desde la posición (pasos/2)+1 hasta pasos
        ns1 = concat [c | (c,i)<-zip sublista1 [0..tamLista sublista1], i==div (tamLista automata) 2]   -- primera parte de ns con las celdas centrales del autómata
        ns2 = concat [c | (c,i)<-zip sublista2 [0..tamLista sublista2], i==div (tamLista automata) 2]   -- segunda parte de ns con las celdas centrales del autómata
        ns = ns1 ++ ns2
        numeroDespl = deListaBinarioANum ns                   -- ns es el número de celdas que se van a desplazar a izquierda o derecha
        nuevosDatosdatosInicialesAleatorios = [ultimo $ init automata, ultimo automata]

calr :: Int -> Int -> [Int] -> [Int] -> [[Int]]
calr regla pasos q0 q1 = [qn1L, qnL]
    where
        inicial = inicializacion (numBits `div` 2) q0 q1
        automata = generaACSO regla 2 pasos inicial
        qnL = automata !! (tamLista automata - 1)
        qn1L = automata !! (tamLista automata - 2)

cac :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
cac regla pasos qn1R qL qn1L qnR = [qn_1C, qnC]
    where
        q = qn1R ++ qn1L
        q' = qL ++ qnR
        inicial = inicializacion numBits q q'
        automata = generaACSO regla 3 pasos inicial
        qn_1C = automata !! (tamLista automata - 1)
        qnC = automata !! (tamLista automata - 2)

{--  Clave de 224 bits:
        - 0-31: CAL rule -> 32 bits para aplicar XOR a CAL
        - 32-63: CAR rule -> 32 bits para aplicar XOR a CAR
        - 64-191: CAC rule -> 128 bits para aplicar XOR a CAC
        - 192-223: CAS rule -> 32 bits para aplicar XOR a CAS -}

rondaTransformaciones :: [[Int]] -> [[Int]] -> [Int] -> Int -> ([[Int]], [[Int]])
rondaTransformaciones datosCAS inicial clave regla = ([q0Final', q1Final'], nuevoInicioCAS')
    where
        datosInicialesAleatorios = primero inicial
        bloqueTextoPlano = ultimo inicial
        parteClaveCAL = slicing clave 0 31
        parteClaveCAR = slicing clave 32 63
        parteClaveCAC = slicing clave 64 191
        parteClaveCAS = divideEnDosSubBloques $ slicing clave 192 223
        subBloquesAleatorios = divideEnDosSubBloques datosInicialesAleatorios
        subBloquesTextoPlano = divideEnDosSubBloques bloqueTextoPlano
        q0L = primero subBloquesAleatorios
        q0R = ultimo subBloquesAleatorios
        q1L = primero subBloquesTextoPlano
        q1R = ultimo subBloquesTextoPlano
        cal = calr regla numPasosCALR q0L q1L
        car = calr regla numPasosCALR q0R q1R
        automataCAS = cas regla numPasosCAS (primero datosCAS) (ultimo datosCAS)
        numDesplazamientoBits1 = fst automataCAS
        -- XOR de las dos últimas configuraciones de CAS con los bits 192-223 de la clave
        primeraConfiguracionXORClave = xorl (primero parteClaveCAS) (primero $ snd automataCAS)
        segundaConfiguracionXORClave = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS)
        -- nuevos datos aleatorios para el CAS (son las dos últimas de automataCAS tras haberle hecho el XOR con la parte correspondiente de la clave)
        nuevoInicioCAS = [primeraConfiguracionXORClave, segundaConfiguracionXORClave]
        qnL = ultimo cal
        qnL' = desplazaBits qnL numDesplazamientoBits1 'L'
        automataCAS' = cas regla numPasosCAS (primero nuevoInicioCAS) (ultimo nuevoInicioCAS)
        numDesplazamientoBits2 = fst automataCAS'
        -- XOR de las dos últimas configuraciones de CAS con los bits 192-223 de la clave
        primeraConfiguracionXORClave' = xorl (primero parteClaveCAS) (primero $ snd automataCAS')
        segundaConfiguracionXORClave' = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS')
        -- nuevos datos aleatorios para el CAS (son las dos últimas de automataCAS tras haberle hecho el XOR con la parte correspondiente de la clave)
        nuevoInicioCAS' = [primeraConfiguracionXORClave', segundaConfiguracionXORClave']
        qnR = ultimo car
        qnR' = desplazaBits qnR numDesplazamientoBits2 'R'
        -- XOR entre qnL y qnR (penúltima evolución del CAR y del CAL) y las partes correspondientes de la clave
        qn1L = ultimo $ init cal
        qn1R = ultimo $ init car
        qn1LXORClave = xorl qn1L parteClaveCAL
        qn1RXORClave = xorl qn1R parteClaveCAR
        automataCAC = cac regla numPasosCAC qn1RXORClave qnL' qn1LXORClave qnR'
        q0Final = ultimo $ init automataCAC
        q1Final = ultimo automataCAC
        -- XOR de los datos finales con los bits 64-191 de la clave
        q0Final' = xorl parteClaveCAC q0Final
        q1Final' = xorl parteClaveCAC q1Final


-- n: rondas a las que se van a someter los datos iniciales y el texto plano para el cifrado
nrondas :: Int -> Int -> [Int] -> [[Int]] -> Int -> ([[Int]], [[Int]])
nrondas semilla n clave inicial regla = nrondas' semilla n clave datosCAS inicial regla []
    where
        listaAleatorios1 = take 16 $ generaAleatoriosL semilla
        listaAleatorios2 = take 16 $ generaAleatoriosL semilla
        datosCAS = [listaAleatorios1, listaAleatorios2]

nrondas' :: Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> Int -> [[Int]] -> ([[Int]], [[Int]])
nrondas' semilla n clave datosCAS inicial regla aux
    | n>0 = nrondas' semilla (n-1) clave nuevosDatosCAS resultadoFinRonda regla (aux ++ resultadoFinRonda)
    | otherwise = ([aux !! (tamLista aux - 2), aux !! (tamLista aux - 1)], nuevosDatosCAS)    --devuelve las dos últimas posiciones, que corresponden al cifrado y a los datos datosInicialesAleatorios
    where
        resultadosRonda = rondaTransformaciones datosCAS inicial clave regla
        resultadoFinRonda = fst resultadosRonda
        nuevosDatosCAS = snd resultadosRonda

cifrado :: Int -> Int -> [Int] -> [[Int]] -> Int -> ([[Int]], [[Int]], [Int])
cifrado semilla n clave bloqueTextoPlano regla = cifrado' semilla n clave restoBloqueTextoPlano regla (fst primerBloqueRondas)
    where
        tamMaximoAC = tamLista $ primero bloqueTextoPlano
        datosInicialesAleatorios = take tamMaximoAC $ generaAleatoriosL semilla
        inicial = [primero bloqueTextoPlano, datosInicialesAleatorios]
        primerBloqueRondas = nrondas semilla n clave inicial regla
        restoBloqueTextoPlano = tail bloqueTextoPlano

cifrado' :: Int -> Int -> [Int] -> [[Int]] -> Int -> [[Int]] -> ([[Int]], [[Int]], [Int])
cifrado' semilla n clave bloqueTextoPlano regla aux
    | n>0 = cifrado' semilla n clave (tail bloqueTextoPlano) regla (fst bloqueRondas ++ aux)
    | otherwise = ([lista | (lista, i)<-zip aux [1..tamLista aux], odd i], snd bloqueRondas, ultimo aux)  -- al final devuelve el par con el texto cifrado y los datos finales aleatorios
    where
        inicial = [primero bloqueTextoPlano, ultimo aux]
        bloqueRondas = nrondas semilla n clave inicial regla

    {----------------------------------------------------------------------
                                Descifrado
    ----------------------------------------------------------------------}

{- Las operaciones se realizan en el orden inverso al del cifrado
        qnL: los bits se desplazan a la derecha 
        qnR: los bits se desplazan a la izquierda -}

-- Devuelve un par con dos listas de listas: el primer elemento está compuesto por la penúltima fila del AC y el segundo está compuesto por la última fila del AC
cacInverso :: Int -> Int -> [Int] -> [Int] -> ([[Int]], [[Int]])
cacInverso regla pasos qnC qn_1C = ([qnL', qnR'], [qn1R, qn1L])
    where
        inicial = inicializacion 64 qnC qn_1C
        automata = generaACSO regla 3 pasos inicial
        qn_1C = automata !! (tamLista automata - 1)
        qnC = automata !! (tamLista automata - 2)
        subbloque1 = divideEnDosSubBloques qnL'
        subbloque2 = divideEnDosSubBloques qn_1C
        qnL' = primero subbloque2
        qn1L = ultimo subbloque1
        qnR' = ultimo subbloque2
        qn1R = primero subbloque1

rondaInversaTransformaciones :: [[Int]] -> [[Int]] -> [Int] -> Int -> ([[Int]], [[Int]])
rondaInversaTransformaciones datosCAS inicial clave regla = ([datosAleatoriosInicial, textoPlano], nuevoInicioCAS')
    where
        datosInicialesAleatorios = primero inicial
        bloqueTextoCifrado = ultimo inicial
        parteClaveCAL = slicing clave 0 31
        parteClaveCAR = slicing clave 32 63
        parteClaveCAC = slicing clave 64 191
        parteClaveCAS = divideEnDosSubBloques $ slicing clave 192 223
        q0Final' = xorl parteClaveCAC bloqueTextoCifrado
        q1Final' = xorl parteClaveCAC datosInicialesAleatorios
        q0XORClave = xorl q0Final' parteClaveCAL
        q1XORClave = xorl q1Final' parteClaveCAR
        automataCAC = cacInverso (reglaReversible 3 regla) numPasosCAC q0XORClave q1XORClave
        qnL' = primero $ fst automataCAC
        qnR' = ultimo $ fst automataCAC
        qn1R = primero $ snd automataCAC
        qn1L = ultimo $ snd automataCAC
        qn1LXORClave = xorl qn1L parteClaveCAL
        qn1RXORClave = xorl qn1R parteClaveCAR
        automataCAS = cas regla numPasosCAS (primero datosCAS) (ultimo datosCAS)
        numDesplazamientoBits1 = fst automataCAS
        primeraConfiguracionXORClave = xorl (primero parteClaveCAS) (primero $ snd automataCAS)
        segundaConfiguracionXORClave = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS)
        nuevoInicioCAS = [primeraConfiguracionXORClave, segundaConfiguracionXORClave]
        qnL = desplazaBits qnL' numDesplazamientoBits1 'R'
        automataCAS' = cas regla numPasosCAS (primero nuevoInicioCAS) (ultimo nuevoInicioCAS)
        numDesplazamientoBits2 = fst automataCAS'
        primeraConfiguracionXORClave' = xorl (primero parteClaveCAS) (primero $ snd automataCAS')
        segundaConfiguracionXORClave' = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS')
        nuevoInicioCAS' = [primeraConfiguracionXORClave', segundaConfiguracionXORClave']
        qnR = desplazaBits qnL' numDesplazamientoBits2 'L'
        cal = calr regla numPasosCALR qnL qn1LXORClave
        car = calr regla numPasosCALR qnR qn1RXORClave
        unionq1 = ultimo cal ++ ultimo car
        unionq0 = primero cal ++ primero car
        datosAleatoriosInicial = unionq0
        textoPlano = unionq1

nrondasInverso :: Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> Int -> [[Int]]
nrondasInverso semilla n clave inicial datosCAS regla = nrondasInverso' semilla n clave inicial datosCAS regla []

nrondasInverso' :: Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> Int -> [[Int]] -> [[Int]]
nrondasInverso' semilla n clave inicial datosCAS regla aux
    | n>0 = nrondasInverso' semilla (n-1) clave resultadoFinRonda nuevosDatosCAS regla (resultadoFinRonda ++ aux)
    | otherwise = [aux !! (tamLista aux - 2), aux !! (tamLista aux - 1)]    --devuelve las dos últimas posiciones, que corresponden al cifrado y a los datos datosInicialesAleatorios
    where
        resultadosRonda = rondaInversaTransformaciones datosCAS inicial clave regla
        resultadoFinRonda = fst resultadosRonda
        nuevosDatosCAS = snd resultadosRonda

descifrado :: Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> [Int] -> Int -> [[Int]]
descifrado semilla n clave datosCAS textoCifrado datosAleatorios regla = descifrado' semilla n clave datosCAS restoTextoCifrado regla primerBloqueRondas
    where
        inicial = [primero textoCifrado, datosAleatorios]
        primerBloqueRondas = nrondasInverso semilla n clave inicial datosCAS regla
        restoTextoCifrado = tail textoCifrado

descifrado' :: Int ->  Int -> [Int] -> [[Int]] -> [[Int]] -> Int -> [[Int]] -> [[Int]]
descifrado' semilla n clave datosCAS textoCifrado regla aux
    | n>0 = descifrado' semilla n clave datosCAS (tail textoCifrado) regla (bloqueRondas ++ aux)
    | otherwise = [lista | (lista, i)<-zip aux [1..tamLista aux], odd i]
    where
        inicial = [primero textoCifrado, ultimo aux]
        bloqueRondas = nrondasInverso semilla n clave inicial datosCAS regla


