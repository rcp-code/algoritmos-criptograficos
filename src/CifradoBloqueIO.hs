module CifradoBloqueIO where

---
import Data.Bits
import Data.Char
import Data.List

---

import Tipos
import AutomataCelular as AC
import UtilGeneral
import UtilCripto
import Constantes
import UtilIO
import GHC.IO (unsafePerformIO)

    {----------------------------------------------------------------------
                Cifrado de bloque basado en AC de segundo orden
    ----------------------------------------------------------------------}

datosInicialesAleatorios :: Int -> [Int]
datosInicialesAleatorios semilla = concat (cambioABase2Lista (generaAleatoriosL semilla))

divideEnDosSubBloques :: [Int] -> [[Int]]
divideEnDosSubBloques [] = error "El bloque de datos esta vacio, no se puede realizar la particion."
divideEnDosSubBloques bloque = [primeraSublista, segundaSublista]
    where
        tam = length bloque - 1
        tamParticion = div tam 2
        primeraSublista = slicing bloque 0 tamParticion
        segundaSublista = slicing bloque (tamParticion + 1) tam

reglaReversible :: Int -> Int -> Int
reglaReversible r1 radioVecindad = abs (2^d - r1 - 1)
    where
        d = 2^(2*radioVecindad+1)

{--  Clave de 224 bits:
        - 0-31: CAL rule -> 32 bits de CAL
        - 32-63: CAR rule -> 32 bits de CAR
        - 64-191: CAC rule -> 128 bits para aplicar XOR a CAC
        - 192-223: CAS rule -> 32 bits para aplicar XOR a CAS -}

inicializaClavePrivada :: [Int] -> ClavePrivada
inicializaClavePrivada semillas = ClavePrivada {k=claveCompleta, kCAL=reglaCAL, kCAR=reglaCAR, kCAC=reglaCAC, kCAS=reglaCAS}
    where 
        numeroAleatorio1 = generaAleatorio (primero semillas) 30 255
        numeroAleatorio2 = generaAleatorio (semillas !! 1) 30 255
        numeroAleatorio3 = generaAleatorio (semillas !! 2) 30 255
        numeroAleatorio4 = generaAleatorio (semillas !! 3) 30 255
        reglaCAL = agregaCerosAIzquierda (cambioABase2 numeroAleatorio1) 32                 --replicate 30 1 ++ [0,0]
        reglaCAR = agregaCerosAIzquierda (cambioABase2 numeroAleatorio2) 32                 --replicate 30 0 ++ [1,0]
        reglaCAC = agregaCerosAIzquierda (cambioABase2 numeroAleatorio3) 128                --replicate 128 1
        reglaCASCompleta = agregaCerosAIzquierda (cambioABase2 numeroAleatorio4) 32         --replicate 30 0 ++ [1,0]
        reglaCAS = divideEnDosSubBloques reglaCASCompleta
        claveCompleta = concat [reglaCAL, reglaCAR, reglaCAC, reglaCASCompleta]
        -- semillaClave = generaAleatorio semilla 0 255
        -- claveCompleta = drop 32 $ agregaCerosAIzquierda (cambioABase2 semillaClave) 256
        -- reglaCAL = take 32 claveCompleta
        -- reglaCAR = take 32 (drop 32 claveCompleta)
        -- reglaCAC = take 128 (drop 64 claveCompleta)
        -- reglaCASCompleta = take 32 (drop 192 claveCompleta)
        -- reglaCAS = divideEnDosSubBloques reglaCASCompleta

    {----------------------------------------------------------------------
                            Versión simplificada
    ----------------------------------------------------------------------}

inicializaACSegundoOrden :: Int -> [Int] -> [Int] -> CycleSO Int
inicializaACSegundoOrden n datos1 datos2 = CycleSO {nCeldas=n, pasado=take n datos1, presente=datos2}

versionSimplificadaCifrado :: [Int] -> [Int] -> Int -> Int -> Int -> [[Int]]
versionSimplificadaCifrado clave texto regla radio pasos = [datosCifrados, datosRes]
    where
        aleatorios = datosInicialesAleatorios 1234
        inicial = inicializaACSegundoOrden (length texto) aleatorios texto
        automata = generaACSO regla radio pasos inicial
        tamAutomata = length automata
        datosResiduales = ultimo automata
        datosCifrados = obtieneSubLista automata (tamAutomata - 2)
        datosRes = xorl clave datosResiduales

versionSimplificadaDescifrado :: [Int] -> [[Int]] -> Int -> Int -> Int -> [[Int]]
versionSimplificadaDescifrado clave datos regla radio pasos = [descifrado, datosInicialesAleatorios]
    where
        datosRes = ultimo datos
        datosCifrados = primero datos
        datosResiduales = xorl clave datosRes
        inicial = inicializaACSegundoOrden (length datosCifrados) datosResiduales datosCifrados                                                   -- radio de vecindad: 1
        automata = generaACSO regla radio pasos inicial
        tamAutomata = length automata
        datosInicialesAleatorios = ultimo automata
        descifrado = obtieneSubLista automata (tamAutomata - 2)


    {----------------------------------------------------------------------
    ----------------------------------------------------------------------
                        Versión más segura de Bouvry 
    ----------------------------------------------------------------------
    ----------------------------------------------------------------------}

    {----------------------------------------------------------------------
                                    Cifrado
    ----------------------------------------------------------------------}

-- Desplaza bits n veces a derecha o izquierda
desplazaBits :: [Int] -> Int -> Char -> [Int]
desplazaBits qn n desplazamiento
    | desplazamiento=='L' = drop n qn ++ take n qn   --desplaza bits a la izquierda
    | desplazamiento=='R' = drop m qn ++ take m qn   --desplaza bits a la derecha
    | otherwise = error "El desplazamiento indicado no es correcto."
    where
        m = length qn - n

-- Función que genera el autómata que obtiene el número de desplazamiento de bits para la operación de desplazamiento (Shift)
cas :: Int -> Int -> [[Int]] -> IO (Int, [[Int]])
cas regla pasos datos = do
    let inicial = inicializacion (numBits `div` 4) (primero datos) (ultimo datos)
    let automata = generaACSO regla 2 pasos inicial
    let tamAutomata = length automata
    let ns = take 5 [elementoCentral f | f<-tail $ tail automata]                           -- primera parte de ns con las celdas centrales del autómata
    let numeroDespl = deListaBinarioANum ns                                                 -- ns es el número de celdas que se van a desplazar a izquierda o derecha
    let nuevoInicioCAS = [automata !! (tamAutomata-2), automata !! (tamAutomata-1)]
    return (numeroDespl, nuevoInicioCAS)

calr :: Int -> Int -> [Int] -> [Int] -> IO [[Int]]
calr regla pasos q0 q1 = do
    let inicial = inicializacion (numBits `div` 2) q0 q1
    let automata = generaACSO regla 2 pasos inicial
    let tam = length automata
    let qnL = ultimo automata
    let qn1L = automata !! (tam-2)
    return [qn1L, qnL]

cac :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int] -> IO [[Int]]
cac regla pasos qn1R qnL qn1L qnR = do
    let q = qn1R ++ qn1L
    let q' = qnL ++ qnR
    let inicial = inicializacion numBits q q'
    let automata = generaACSO regla 3 pasos inicial
    let tam = length automata
    let qn_1C = automata !! (tam-2)
    let qnC = ultimo automata
    return [qn_1C, qnC]

-- Proceso inicial de la ronda de transformaciones
procesoInicial :: [[Int]] -> ([[Int]], [[Int]])
procesoInicial inicial = (datosAleatorios, texto)
    where
        datosAleatorios = divideEnDosSubBloques $ primero inicial
        texto = divideEnDosSubBloques $ ultimo inicial

-- Proceso que genera el autómata CAS
procesoAutomataS :: Int -> [[Int]] -> IO (Int, [[Int]])
procesoAutomataS regla = cas regla numPasosCAS

-- Proceso que genera el autómata izquierdo y derecho y devuelve el par con las dos últimas configuraciones del izquierdo y el derecho
procesoAutomataLR :: Int -> Int -> [[Int]] -> [[Int]] -> ([[Int]], [[Int]])
procesoAutomataLR reglaL reglaR aleatorios texto = (unsafePerformIO configAutomataIzquierdo, unsafePerformIO configAutomataDerecho)
    where
        configAutomataIzquierdo = calr reglaL numPasosCALR (primero aleatorios) (primero texto)
        configAutomataDerecho = calr reglaR numPasosCALR (ultimo aleatorios) (ultimo texto)

procesoAutomataC :: Int -> ([[Int]], [[Int]]) -> Int -> IO [[Int]]
procesoAutomataC regla (confIzquierda, confDerecha) desplazamientoBits = do 
    let qnL' = desplazaBits (ultimo confIzquierda) desplazamientoBits 'L'
    let qnR' = desplazaBits (ultimo confDerecha) desplazamientoBits 'R'
    let qn1L = primero confIzquierda
    let qn1R = primero confDerecha
    cac regla numPasosCAC qn1R qnL' qn1L qnR'
        

-- Ronda individual de transformaciones de datos dentro del proceso de cifrado
rondaTransformaciones :: [[Int]] -> [[Int]] -> ClavePrivada -> IO ([[Int]], [[Int]])
rondaTransformaciones datosCAS inicial clave = do
    let reglaCAS = deListaBinarioANum $ concat (kCAS clave)
    let reglaCAL = deListaBinarioANum (kCAL clave)
    let reglaCAR = deListaBinarioANum (kCAR clave)
    let reglaCAC = deListaBinarioANum (kCAC clave)
    let (subBloquesAleatorios, subBloquesTextoPlano) = procesoInicial inicial
    let (numDesplazamientoBits, nuevoInicioCAS) = unsafePerformIO $ procesoAutomataS reglaCAS datosCAS         
    let (confIzquierda, confDerecha) = procesoAutomataLR reglaCAL reglaCAR subBloquesAleatorios subBloquesTextoPlano
    automataCAC <- procesoAutomataC reglaCAC (confIzquierda, confDerecha) numDesplazamientoBits
    let textoCifrado = ultimo automataCAC
    let datosResiduales = primero automataCAC
    return ([textoCifrado, datosResiduales], nuevoInicioCAS)

-- n rondas a las que se van a someter los datos iniciales y el texto plano para el cifrado
nrondas :: Int -> ClavePrivada -> [[Int]] -> [[Int]] -> IO ([[Int]], [[Int]])
nrondas 0 _ inicial datosCAS = do return (inicial, datosCAS)
nrondas rondas clave inicial datosCAS = do
    nrondas (rondas-1) clave configuracionesTextoYresiduo nuevoCAS
    where
        (configuracionesTextoYresiduo, nuevoCAS) = unsafePerformIO $ rondaTransformaciones datosCAS inicial clave

-- Función que reúne todas las rondas durante el cifrado
cifrado :: [[Int]] -> IO ([[Int]], [Int], [[Int]], ClavePrivada)
cifrado bloquesTextoClaro = do
    cifrado' numeroRondas clave restoBloquesTextoPlano (acumuladoTextoCifrado, residuoCifrado, nuevoCAS)
    where 
        -- Inicialización de la clave privada
        n1 = unsafePerformIO creaNumeroAleatorioMedianteAC
        n2 = unsafePerformIO creaNumeroAleatorioMedianteAC
        n3 = unsafePerformIO creaNumeroAleatorioMedianteAC
        semillaClave1 = unsafePerformIO creaNumeroAleatorioMedianteAC
        semillaClave2 = unsafePerformIO creaNumeroAleatorioMedianteAC
        semillaClave3 = unsafePerformIO creaNumeroAleatorioMedianteAC
        semillaClave4 = unsafePerformIO creaNumeroAleatorioMedianteAC
        semillas = [n1,n2,n3]
        semillasClaves = [semillaClave1, semillaClave2, semillaClave3, semillaClave4]
        clave = inicializaClavePrivada semillasClaves
        -- 
        primeraConfiguracionACR = primero $ divideEnDosSubBloques $ kCAC clave
        inicial = [primeraConfiguracionACR, primero bloquesTextoClaro]
        listaAleatoriosCAS1 = primero (kCAS clave)
        listaAleatoriosCAS2 = ultimo (kCAS clave)
        datosCAS = [listaAleatoriosCAS1, listaAleatoriosCAS2]
        (datosCifrados, nuevoCAS) = unsafePerformIO $ nrondas numeroRondas clave inicial datosCAS
        acumuladoTextoCifrado = [primero datosCifrados]
        residuoCifrado = ultimo datosCifrados
        restoBloquesTextoPlano = tail bloquesTextoClaro

cifrado' :: Int -> ClavePrivada -> [[Int]] -> ([[Int]], [Int], [[Int]]) -> IO ([[Int]], [Int], [[Int]], ClavePrivada)
cifrado' rondas clave bloquesTextoClaro (acumuladoTextoCifrado, residuoCifrado, cas)
    | null bloquesTextoClaro = do 
        let cifradoCASCompletoXORClave = procesoCifradoCAS clave cas
        let residualesXORClave = xorl (kCAC clave) residuoCifrado
        return (acumuladoTextoCifrado, residualesXORClave, cifradoCASCompletoXORClave, clave)
    | otherwise = do
        let inicial = [residuoCifrado, primero bloquesTextoClaro]
        (datosCifrados, nuevoCAS) <- nrondas numeroRondas clave inicial cas 
        let textoCifrado = primero datosCifrados
        let residuoCifradoActualizado = ultimo datosCifrados
        cifrado' rondas clave (tail bloquesTextoClaro) (acumuladoTextoCifrado ++ [textoCifrado], residuoCifradoActualizado, nuevoCAS)
   

    {----------------------------------------------------------------------
                                Descifrado
    ----------------------------------------------------------------------}

{- Las operaciones se realizan en el orden inverso al del cifrado
        qnL: los bits se desplazan a la derecha 
        qnR: los bits se desplazan a la izquierda -}

casInverso :: Int -> Int -> [[Int]] -> IO (Int, [[Int]])
casInverso regla pasos datos = do
    let inicial = inicializacion (numBits `div` 4) (primero datos) (ultimo datos)
    let automata = generaACSO regla 2 pasos inicial
    let tamAutomata = length automata
    let ns = reverse $ take 5 $ map elementoCentral automata
    let numeroDespl = deListaBinarioANum ns                                                         -- ns es el número de celdas que se van a desplazar a izquierda o derecha
    let nuevoInicioCAS = [automata !! (tamAutomata-2), automata !! (tamAutomata-1)]
    return (numeroDespl, nuevoInicioCAS)

-- Devuelve un par con dos listas de listas: el primer elemento está compuesto por la penúltima fila del AC y el segundo está compuesto por la última fila del AC
cacInverso :: Int -> [Int] -> [Int] -> IO ([[Int]], [[Int]])
cacInverso regla qnC qn1C = do 
    let inicial = inicializacion numBits qnC qn1C
        automata = generaACSO regla 3 numPasosCAC inicial
        qn1LR = automata !! (length automata - 1)
        qnLR = automata !! (length automata - 2)
        subbloques2 = divideEnDosSubBloques qn1LR
        subbloques1 = divideEnDosSubBloques qnLR
        qnL1L = primero subbloques1 : [primero subbloques2]
        qnR1R = ultimo subbloques1 : [ultimo subbloques2]
    return (qnL1L, qnR1R) --izquierda y derecha

procesoAutomataSInverso :: Int -> [[Int]] -> IO (Int, [[Int]])
procesoAutomataSInverso regla = casInverso regla numPasosCAS

-- Realiza el proceso de transformaciones del CAC, de forma inversa
procesoAutomataCInverso :: Int -> [[Int]] -> ([[Int]], [[Int]])
procesoAutomataCInverso regla confInicial = unsafePerformIO $ cacInverso regla (primero confInicial) (ultimo confInicial)

-- Realiza el proceso de transformaciones relacionadas con los autómatas izquierdo y derecho
procesoAutomataLRInverso :: Int -> Int -> ([[Int]], [[Int]]) -> Int -> IO ([[Int]], [[Int]])
procesoAutomataLRInverso reglaL reglaR (confIzquierda, confDerecha) desplazamientoBits = do
    let qnL = desplazaBits (primero confIzquierda) desplazamientoBits 'R'
        qnR = desplazaBits (primero confDerecha) desplazamientoBits 'L'
        qn1L = ultimo confIzquierda
        qn1R = ultimo confDerecha
    configAutomataIzquierdo <- calr reglaL numPasosCALR qnL qn1R
    configAutomataDerecho <- calr reglaR numPasosCALR qnR qn1L
    return (configAutomataIzquierdo, configAutomataDerecho)

-- Ronda de transformaciones de datos. Parecida a la del cifrado, pero a la inversa
rondaInversaTransformaciones :: [[Int]] -> [[Int]] -> ClavePrivada -> IO ([[Int]], [[Int]])
rondaInversaTransformaciones datosCAS inicial clave = do
    let reglaCAS = deListaBinarioANum $ concat (kCAS clave)
    let reglaCAL = deListaBinarioANum (kCAL clave)
    let reglaCAR = deListaBinarioANum (kCAR clave)
    let reglaCAC = deListaBinarioANum (kCAC clave)
    let (numeroDesplazamiento, nuevoInicioCAS) = unsafePerformIO $ procesoAutomataSInverso reglaCAS datosCAS            
    let (confIzquierda, confDerecha) = procesoAutomataCInverso reglaCAC inicial                                  
    let (configAutomataIzquierdo, configAutomataDerecho) = unsafePerformIO $ procesoAutomataLRInverso reglaCAL reglaCAR (confIzquierda, confDerecha) numeroDesplazamiento  --CAMBIADA LA REGLA
    let textoDescifrado = ultimo configAutomataIzquierdo ++ ultimo configAutomataDerecho
    let datosAleatoriosIniciales = primero configAutomataIzquierdo ++ primero configAutomataDerecho
    return ([textoDescifrado, datosAleatoriosIniciales], nuevoInicioCAS)

nrondasInverso :: Int -> ClavePrivada -> [[Int]] -> [[Int]] -> IO ([[Int]], [[Int]])
nrondasInverso 0 _ inicial datosCAS = do return (inicial, datosCAS)
nrondasInverso rondas clave inicial datosCAS = do 
    nrondasInverso (rondas-1) clave configuracionesTextoYresiduo nuevoCAS
    where
        (configuracionesTextoYresiduo, nuevoCAS) = unsafePerformIO $ rondaInversaTransformaciones datosCAS inicial clave

descifrado :: ClavePrivada -> [[Int]] -> [[Int]] -> [Int] -> IO ([[Int]], [Int], [[Int]])
descifrado clave datosCAS textoCifrado datosResiduales = do
    descifrado' numeroRondas clave restoTextoCifrado (textoDescifrado, residuoDescifrado, nuevoCAS)
    where
        bloquesTextoCifrado = reverse textoCifrado
        descifradoDatosAleatorios = xorl (kCAC clave) datosResiduales
        datosCAS' = procesoDescifradoDatosCAS clave datosCAS
        inicial = [primero bloquesTextoCifrado, descifradoDatosAleatorios]
        (datosDescifrados, nuevoCAS) = unsafePerformIO $ nrondasInverso numeroRondas clave inicial datosCAS'
        textoDescifrado = [ultimo datosDescifrados]
        residuoDescifrado = primero datosDescifrados
        restoTextoCifrado = tail bloquesTextoCifrado

descifrado' :: Int -> ClavePrivada -> [[Int]] -> ([[Int]], [Int], [[Int]]) -> IO ([[Int]], [Int], [[Int]])
descifrado' rondas clave bloquesTextoCifrado (acumuladoTextoDescifrado, residuoDescifrado, cas)
    | null bloquesTextoCifrado = do 
        return (reverse acumuladoTextoDescifrado, residuoDescifrado, cas)
    | otherwise = do descifrado' rondas clave (tail bloquesTextoCifrado) (acumuladoTextoDescifrado ++ [textoDescifrado], residuoDescifradoActualizado, nuevoCAS)
    where
        inicial = [primero bloquesTextoCifrado, residuoDescifrado]
        (datosDescifrados, nuevoCAS) = unsafePerformIO $ nrondasInverso rondas clave inicial cas
        textoDescifrado = ultimo datosDescifrados
        residuoDescifradoActualizado | null bloquesTextoCifrado = residuoDescifrado | otherwise = primero datosDescifrados

    {----------------------------------------------------------------------
                                Preparativos
    ----------------------------------------------------------------------}

preparaTexto :: Mensaje -> [[Int]]
preparaTexto mensaje
    | length listas8Bits <= 8 = if length ultimoByte < 64 then [ultimoByte'] else listas8Bytes
    | otherwise = if length ultimoByte < 64 then init listas8Bytes ++ [ultimoByte'] else listas8Bytes
    where
        listas8Bits = map deIntA8Bits (transformaTextoEnEntero mensaje)
        listas8Bytes = de8BitsA1Byte listas8Bits
        ultimoByte = ultimo listas8Bytes
        ultimoByte' = agregaCerosAlFinal ultimoByte

cambiaListasEnterosATexto :: [[Int]] -> Mensaje
cambiaListasEnterosATexto bloques = transformaEnteroEnTexto listaNumerosCaracteresSinCeros
    where
        divideEn8Bloques = concatMap separaOctetos bloques
        listaNumerosCaracteres = map deListaBinarioANum divideEn8Bloques
        listaNumerosCaracteresSinCeros = reverse (dropWhile (==0) (reverse listaNumerosCaracteres))

cambiaATexto :: [Int] -> Mensaje
cambiaATexto bloque = transformaEnteroEnTexto listaNumerosCaracteresSinCeros
    where 
        divideEn8Bloques = separaOctetos bloque
        listaNumerosCaracteres = map deListaBinarioANum divideEn8Bloques
        listaNumerosCaracteresSinCeros = reverse (dropWhile (==0) (reverse listaNumerosCaracteres))

procesoCifradoCAS :: ClavePrivada -> [[Int]] -> [[Int]]
procesoCifradoCAS clave datosCAS = cifradoCASCompletoXORClave
    where 
        cifradoCAS1XORClave = xorl (primero $ kCAS clave) (primero datosCAS)
        cifradoCAS2XORClave = xorl (ultimo $ kCAS clave) (ultimo datosCAS)
        cifradoCASCompletoXORClave = cifradoCAS1XORClave : [cifradoCAS2XORClave]

procesoDescifradoDatosCAS :: ClavePrivada -> [[Int]] -> [[Int]]
procesoDescifradoDatosCAS clave datosCAS = descifradoCASCompletoXORClave
    where
        -- Descifrado para obtener las dos configuraciones de CAS:
        descifradoCAS1XORClave = xorl (primero $ kCAS clave) (primero datosCAS)
        descifradoCAS2XORClave = xorl (ultimo $ kCAS clave) (ultimo datosCAS)
        descifradoCASCompletoXORClave = descifradoCAS2XORClave : [descifradoCAS1XORClave]

    {----------------------------------------------------------------------
                        Pruebas del cifrado/descifrado
    ----------------------------------------------------------------------}

main :: IO ()
main = do
    imprime "--------------------------------------------------------------------------------------"
    imprime "Versión simplificada del cifrado de bloque basado en AC de segundo orden (vecindad 1):"
    imprime "--------------------------------------------------------------------------------------"
    main1
    imprime "--------------------------------------------------------------------------------------"
    imprime "Versión simplificada del cifrado de bloque basado en AC de segundo orden (vecindad 2):"
    imprime "--------------------------------------------------------------------------------------"
    main2
    imprime "--------------------------------------------------------------------------------------"
    imprime "Versión simplificada del cifrado de bloque basado en AC de segundo orden (vecindad 3):"
    imprime "--------------------------------------------------------------------------------------"
    main3
    imprime "--------------------------------------------------------------------------------------"
    imprime "Cifrado de bloque basado en AC de segundo orden:"
    imprime "--------------------------------------------------------------------------------------"
    main4'
    imprime "--------------------------------------------------------------------------------------"

-- :set +s 
-- main

main1 :: IO Mensaje
main1 = do
    semilla <- now
    imprime "Introduce el texto que quieres cifrar: "
    texto <- getLine
    let textoPreparado = concat $ preparaTexto texto
    let clave = datosInicialesAleatorios 9123
    let procesoCifrado = versionSimplificadaCifrado clave textoPreparado reglaAC 1 numPasos
    let textoCifrado = primero procesoCifrado
    let residuales = ultimo procesoCifrado
    let procesoDescifrado = versionSimplificadaDescifrado clave procesoCifrado reglaAC 1 numPasos
    let textoDescifrado = primero procesoDescifrado
    imprime ("  El texto codificado es: " ++ show textoPreparado)
    imprime ("  El texto cifrado es: " ++ show textoCifrado)
    imprime ("  El texto descifrado es: " ++ show textoDescifrado)
    let iguales = textoPreparado == textoDescifrado
    if iguales then do
        imprime "   El texto se ha descifrado correctamente."
    else do
        imprime "   El texto NO se ha descifrado bien."
    return (cambiaATexto textoDescifrado)

main2 :: IO Mensaje
main2 = do
    semilla <- now
    imprime "Introduce el texto que quieres cifrar: "
    texto <- getLine
    let textoPreparado = concat $ preparaTexto texto
    let clave = datosInicialesAleatorios 9123
    let procesoCifrado = versionSimplificadaCifrado clave textoPreparado reglaAC 2 numPasos
    let textoCifrado = primero procesoCifrado
    let residuales = ultimo procesoCifrado
    let procesoDescifrado = versionSimplificadaDescifrado clave procesoCifrado reglaAC 2 numPasos
    let textoDescifrado = primero procesoDescifrado
    imprime ("  El texto codificado es: " ++ show textoPreparado)
    imprime ("  El texto cifrado es: " ++ show textoCifrado)
    imprime ("  El texto descifrado es: " ++ show textoDescifrado)
    let iguales = textoPreparado == textoDescifrado
    if iguales then do
        imprime "   El texto se ha descifrado correctamente."
    else do
        imprime "   El texto NO se ha descifrado bien."
    return (cambiaATexto textoDescifrado)

main3 :: IO Mensaje
main3 = do
    semilla <- now
    imprime "Introduce el texto que quieres cifrar: "
    texto <- getLine
    let textoPreparado = concat $ preparaTexto texto
    let clave = datosInicialesAleatorios 9123
    let procesoCifrado = versionSimplificadaCifrado clave textoPreparado reglaAC 3 numPasos
    let textoCifrado = primero procesoCifrado
    let residuales = ultimo procesoCifrado
    let procesoDescifrado = versionSimplificadaDescifrado clave procesoCifrado reglaAC 3 numPasos
    let textoDescifrado = primero procesoDescifrado
    imprime ("  El texto codificado es: " ++ show textoPreparado)
    imprime ("  El texto cifrado es: " ++ show textoCifrado)
    imprime ("  El texto descifrado es: " ++ show textoDescifrado)
    let iguales = textoPreparado == textoDescifrado
    if iguales then do
        imprime "   El texto se ha descifrado correctamente."
    else do
        imprime "   El texto NO se ha descifrado bien."
    return (cambiaATexto textoDescifrado)

main4' :: IO ()
main4' = do
    putStrLn "  Introduzca el texto a cifrar:"
    texto <- getLine
    let textoCodificado = preparaTexto texto
    let procesoCifrado = unsafePerformIO $ cifrado textoCodificado
    let textoCifrado = fst'' procesoCifrado
    let datosResiduales = snd'' procesoCifrado
    let datosCASFinalesCifrado = trd'' procesoCifrado
    let clave = frt'' procesoCifrado
    let textoDescifrado = unsafePerformIO $ descifrado clave datosCASFinalesCifrado textoCifrado datosResiduales
    let textoDescifradoDescodificado = cambiaListasEnterosATexto (fst' textoDescifrado)
    let controlTexto = textoCodificado == fst' textoDescifrado
    imprime ("  El texto es: " ++ texto)
    imprime ("  El texto codificado es: " ++ show (map deListaBinarioANum textoCodificado))
    imprime ("  La clave es: " ++ show (deListaBinarioANum (k clave)))
    imprime ("  Datos iniciales 'aleatorios': " ++ show (deListaBinarioANum (kCAC clave)))
    imprime ("  Texto cifrado: " ++ show (map deListaBinarioANum textoCifrado))
    imprime ("  Datos residuales finales: " ++ show (deListaBinarioANum datosResiduales))
    imprime ("  Texto descifrado sin codificar: " ++ show (map deListaBinarioANum (fst' textoDescifrado)))
    imprime ("  Texto descifrado: " ++ show textoDescifradoDescodificado)
    putStr "    ¿El texto se ha descifrado correctamente? "
    if controlTexto then do
        imprime "Sí."
    else do
        imprime "No."


main5 :: IO ()
main5 = do
    semillaCAS1 <- now
    semillaClave1 <- creaNumeroAleatorioMedianteAC
    semillaClave2 <- creaNumeroAleatorioMedianteAC
    semillaClave3 <- creaNumeroAleatorioMedianteAC
    semillaClave4 <- creaNumeroAleatorioMedianteAC
    semillaCAS2 <- creaNumeroAleatorioMedianteAC
    let semillasClaves = [semillaClave1, semillaClave2, semillaClave3, semillaClave4]
    let clave = inicializaClavePrivada semillasClaves
    let divisionClave = divideEnDosSubBloques (kCAC clave)
    let listaAleatorios1 = take 16 $ concat $ cambioABase2Lista $ generaAleatoriosL semillaCAS1
    let listaAleatorios2 = take 16 $ concat $ cambioABase2Lista $ generaAleatoriosL semillaCAS2
    let datosCAS = [listaAleatorios1, listaAleatorios2]    
    let cifradoCAS = procesoCifradoCAS clave datosCAS
    let descifradoCAS = procesoDescifradoDatosCAS clave cifradoCAS
    imprime ("La clave (parte CAS) es: " ++ show (kCAS clave))
    imprime ("La clave (parte CAC) dividida en dos tiene tamaño: " ++ show (length (kCAC clave)))
    imprime ("Datos iniciales del CAS: " ++ show datosCAS ++ " \n cifrado como: " ++ show cifradoCAS)
    imprime ("Se descifra como: " ++ show descifradoCAS)