import UtilIO
import UtilGeneral
import Constantes
import AutomataCelular
import Test.QuickCheck
import TestsPropiedades


main :: IO ()
main = do
    imprime ""
    imprime "           Bienvenido/a al entorno de pruebas..."  
    imprime ""
    imprime "-------------Propiedades generales-------------"
    imprime ""
    imprime "Propiedades de entero a binario (y viceversa): "
    imprime ""
    quickCheck prop_entero_binario
    imprime ""
    imprime "Propiedades de número a lista de dígitos (y viceversa): "
    imprime ""
    quickCheck prop_numeroAdigitos
    imprime ""
    imprime "Propiedades de codificación y descodificación en binario: "
    imprime ""
    quickCheck prop_codifica_descodifica
    imprime ""
    imprime "-------------Propiedades relativas al cifrado RSA-------------"
    imprime ""
    imprime "Propiedad de clave privada: "
    imprime ""
    quickCheck prop_clavePrivada
    imprime ""
    imprime "Propiedad de la exponenciación modular"
    imprime ""
    quickCheck prop_exponenciacionModular
    imprime ""
    imprime "Propiedad que comprueba que el mensaje original es el mismo al descifrar el texto cifrado."
    imprime ""
    quickCheck prop_cifradoRSA_descifradoRSA
    imprime ""
    imprime "-------------Propiedades relativas al cifrado de Wolfram-------------"
    imprime ""
    imprime "Propiedad de clave privada: "
    imprime ""
    quickCheck prop_cifrado_descifrado_Wolfram
    imprime ""
    imprime "-------------Propiedades relativas a autómatas celulares-------------"
    imprime ""
    imprime "Propiedad autómatas reversibles: "
    imprime ""
    quickCheck prop_automata_automataR''
    imprime ""
    imprime ""
    imprime "-------------Propiedades relativas a cifrado de bloques basado en AC de segundo orden-------------"
    imprime ""
    quickCheck prop_cifrado_descifrado_conAutomatasDeSegundoOrden
    imprime "Fin de la ejecución."

