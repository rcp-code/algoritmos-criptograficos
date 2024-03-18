module CifradoBloque where


import Tipos
import AutomataCelular as AC
import UtilGeneral
import UtilCripto
---
import Data.Bits
import Data.Char
import Data.List

    {----------------------------------------------------------------------
                Cifrado de bloque basado en AC de segundo orden
    ----------------------------------------------------------------------}

datosdatosInicialesAleatorios :: Int -> [Int]
datosdatosInicialesAleatorios semilla = concat (cambioABase2Lista (generaAleatoriosL semilla))

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
inicializaACSegundoOrden n datos1 datos2 = CycleSO {nCeldas=n, pasado=datos1, presente=datos2}

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
versionSimplificadaDescifrado clave datos regla radio pasos = [descifrado, datosdatosInicialesAleatorios]
    where
        claveFormaLista = digitos clave
        datosResiduales = ultimo datos
        datosCifrados = primero datos
        datosRes = xorl claveFormaLista datosResiduales
        inicial = inicializaACSegundoOrden (tamLista datosResiduales) datosRes datosCifrados
        reglaRev = reglaReversible regla 1                                                      -- radio de vecindad: 1
        automata = generaACSO reglaRev radio pasos inicial
        tamAutomata = tamLista automata
        datosdatosInicialesAleatorios = ultimo automata
        descifrado = obtieneSubLista automata (tamAutomata - 2)


    {----------------------------------------------------------------------
                        Versión más segura de Bouvry 
    ----------------------------------------------------------------------}

-- Desplaza bits n veces a derecha o izquierda
desplazaBits :: [Int] -> Int -> Char -> [Int]
desplazaBits qn n desplazamiento
    | desplazamiento=='L' = drop n qn ++ take n qn   --desplaza bits a la izquierda
    | desplazamiento=='R' = drop m qn ++ take m qn   --desplaza bits a la derecha
    | otherwise = error "El desplazamiento no es correcto."
    where
        m = length qn - n

-- Tamaño de bloque de 16 bits y radio de vecindad 2
cas :: Int -> Int -> [Int] -> [Int] -> (Int, [[Int]])
cas regla pasos datos1 datos2 = (ns, nuevosDatosdatosInicialesAleatorios)
    where
        inicial = inicializacion 16 datos1 datos2
        automata = generaACSO regla 0 pasos inicial            
        p = div pasos 2                                        -- posición: pasos/2
        sublista1 = slicing automata 2 p                       -- sublista desde la posición 2 hasta p
        sublista2 = slicing automata (p+1) pasos               -- sublista desde la posición (pasos/2)+1 hasta pasos
        ns1 = concat [c | (c,i)<-zip sublista1 [1..tamLista sublista1], i==div (tamLista automata) 2]   -- primera parte de ns con las celdas centrales del autómata
        ns2 = concat [c | (c,i)<-zip sublista2 [1..tamLista sublista2], i==div (tamLista automata) 2]   -- segunda parte de ns con las celdas centrales del autómata
        ns = deListaBinarioANum (ns1 ++ ns2)                   -- ns es el número de celdas que se van a desplazar a izquierda o derecha
        nuevosDatosdatosInicialesAleatorios = [ultimo $ init automata, ultimo automata]

calr :: Int -> Int -> [Int] -> [Int] -> [[Int]]
calr regla pasos q0 q1 = [qn_1L, qnL]
    where
        inicial = inicializacion 32 q0 q1
        automata = generaACSO regla 2 pasos inicial
        qnL = automata !! (tamLista automata - 1)
        qn_1L = automata !! (tamLista automata - 2)

cac :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
cac regla pasos qn_1R qL qn_1L qnR = [qn_1C, qnC]
    where
        q = qn_1R ++ qn_1L
        q' = qL ++ qnR
        inicial = inicializacion 64 q q'
        automata = generaACSO regla 3 pasos inicial
        qn_1C = automata !! (tamLista automata - 1)
        qnC = automata !! (tamLista automata - 2)

{--  Clave de 224 bits:
        - 0-31: CAL rule -> 32 bits para aplicar XOR a CAL
        - 32-63: CAR rule -> 32 bits para aplicar XOR a CAR
        - 64-191: CAC rule -> 128 bits para aplicar XOR a CAC
        - 192-223: CAS rule -> 32 bits para aplicar XOR a CAS -}

rondaTransformaciones :: [[Int]] -> [[Int]] -> [Int] -> Int -> Int -> ([[Int]], [[Int]])
rondaTransformaciones datosCAS inicial clave regla pasos = rondaTransformaciones nuevoInicioCAS' (fst agrupa) clave regla (pasos-1)
    where 
        numPasosCALR = 50
        numPasosCAC = 100
        datosInicialesAleatorios = primero inicial
        bloqueTextoPlano = ultimo inicial
        parteClaveCAL = slicing clave 0 31
        parteClaveCAR = slicing clave 32 63
        parteClaveCAC = slicing clave 64 191
        parteClaveCAS = divideEnDosSubBloques $ slicing clave 192 223
        q0L = primero $ divideEnDosSubBloques datosInicialesAleatorios 
        q0R = ultimo $ divideEnDosSubBloques datosInicialesAleatorios 
        q1L = primero $ divideEnDosSubBloques bloqueTextoPlano 
        q1R = ultimo $ divideEnDosSubBloques bloqueTextoPlano
        cal = calr regla numPasosCALR q0L q1L 
        car = calr regla numPasosCALR q0R q1R
        automataCAS = cas regla 10 (primero datosCAS) (ultimo datosCAS)
        n1 = fst automataCAS       
        primeraConfiguracionXORClave = xorl (primero parteClaveCAS) (primero $ snd automataCAS)
        segundaConfiguracionXORClave = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS)
        nuevoInicioCAS = [primeraConfiguracionXORClave, segundaConfiguracionXORClave]
        qnL = ultimo cal
        qnL' = desplazaBits qnL n1 'L'
        qnR = ultimo car 
        automataCAS' = cas regla 10 (primero nuevoInicioCAS) (ultimo nuevoInicioCAS)
        n2 = fst automataCAS'
        -- XOR de las dos últimas configuraciones de CAS con los bits 192-223 de la clave
        primeraConfiguracionXORClave' = xorl (primero parteClaveCAS) (primero $ snd automataCAS')
        segundaConfiguracionXORClave' = xorl (ultimo parteClaveCAS) (ultimo $ snd automataCAS')
        nuevoInicioCAS' = [primeraConfiguracionXORClave', segundaConfiguracionXORClave']
        qnR' = desplazaBits qnR n2 'R'
        automataCAC = cac regla numPasosCAC (ultimo $ init car) qnL' (ultimo $ init cal) qnR'
        q0Final = ultimo $ init automataCAC
        q1Final = ultimo automataCAC
        -- XOR de los datos finales con los bits 64-191 de la clave
        q0Final' = xorl parteClaveCAC q0Final
        q1Final' = xorl parteClaveCAC q1Final
        agrupa = ([q0Final', q1Final'], nuevoInicioCAS')


-- n: rondas a las que se van a someter los datos iniciales y el texto plano para el cifrado
nrondas :: Int -> Int -> [Int] -> [[Int]] -> Int -> Int -> [[Int]]
nrondas semilla n clave inicial regla pasos = nrondas' semilla n clave datosCAS inicial regla pasos []
    where
        listaAleatorios1 = take 16 $ generaAleatoriosL semilla
        listaAleatorios2 = take 16 $ generaAleatoriosL semilla
        datosCAS = [listaAleatorios1, listaAleatorios2]

nrondas' :: Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
nrondas' semilla n clave datosCAS inicial regla pasos aux
    | n>0 = nrondas' semilla (n-1) clave nuevosDatosCAS resultadoFinRonda regla pasos (aux ++ resultadoFinRonda)
    | otherwise = [aux !! (tamLista aux - 2), aux !! (tamLista aux - 1)]    --devuelve las dos últimas posiciones, que corresponden al cifrado y a los datos datosInicialesAleatorios
    where
        resultadosRonda = rondaTransformaciones datosCAS inicial clave regla pasos
        resultadoFinRonda = fst resultadosRonda
        nuevosDatosCAS = snd resultadosRonda

cifrado :: Int -> Int -> [Int] -> [[Int]] -> Int -> Int -> ([[Int]], [Int])
cifrado semilla n clave bloqueTextoPlano regla pasos = cifrado' semilla n clave restoBloqueTextoPlano regla pasos primerBloqueRondas
    where 
        tamMaximoAC = tamLista $ primero bloqueTextoPlano
        datosInicialesAleatorios = take tamMaximoAC $ generaAleatoriosL semilla
        listaAleatorios1 = take 16 $ generaAleatoriosL semilla
        listaAleatorios2 = take 16 $ generaAleatoriosL semilla
        inicioAleatorioCAS = [listaAleatorios1, listaAleatorios2]
        inicial = [primero bloqueTextoPlano, datosInicialesAleatorios]
        primerBloqueRondas = nrondas semilla n clave inicial regla pasos
        restoBloqueTextoPlano = tail bloqueTextoPlano

cifrado' :: Int -> Int -> [Int] -> [[Int]] -> Int -> Int -> [[Int]] -> ([[Int]], [Int])
cifrado' semilla n clave bloqueTextoPlano regla pasos aux
    | n>0 = cifrado' semilla n clave (tail bloqueTextoPlano) regla pasos (aux ++ bloqueRondas)
    | otherwise = ([lista | (lista, i)<-zip aux [1..tamLista aux], odd i], ultimo aux)  -- al final devuelve el par con el texto cifrado y los datos finales aleatorios
    where
        inicial = [primero bloqueTextoPlano, ultimo aux]
        bloqueRondas = nrondas semilla n clave inicial regla pasos  

descifrado :: Int -> [Int] -> [[Int]] -> Int -> Int -> [[Int]]
descifrado n clave bloqueTextoPlano regla pasos = undefined