module Pruebas where


import UtilIO
import UtilGeneral
import Constantes
import AutomataCelularSO


mainPruebas :: IO ()
mainPruebas = do
    imprime "Cargando..."
    pruebaAutomataSO
    imprime "Fin de la ejecución."

    {----------------------------------------------------------------------
                                    Pruebas
    ----------------------------------------------------------------------}

-- pruebaAutomata :: IO ()
-- pruebaAutomata = do
--     imprime "Selecciona una regla para el autómata celular: "
--     r <- leeMensaje
--     semillaLista <- now
--     semillaCeldas <- now
--     let numCeldasAlt = generaAleatorio semillaCeldas minCeldas maxCeldas        
--     let numCeldas | even numCeldasAlt = numCeldasAlt + 1                         
--                   | otherwise = numCeldasAlt
--     let m0 = "El total de celdas es: " ++ show numCeldas
--     imprime m0
--     let listaAleatorios = generaAleatoriosL semillaLista
--     let lAleatBase2 = concat (cambioABase2Lista listaAleatorios)
--     let inicia = iniciaAC numCeldas lAleatBase2
--     let automata = generaAC numCeldas (regla (deStringAInt r)) inicia
--     muestraAC numCeldas (regla (deStringAInt r)) inicia

pruebaAutomataSO :: IO ()
pruebaAutomataSO = do
    semilla <- now
    let listaAleatoria = inicialAleatorio semilla
    let numeroAleatorio = deListaBinarioANum $ estadoFinal listaAleatoria
    imprime "El número aleatorio generado es: "
    imprime $ show numeroAleatorio