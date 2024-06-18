# Algoritmos criptográficos con autómatas celulares en Haskell

## Descripción

Este proyecto no solo ha sido motivado por mi TFG (Trabajo de Fin de Grado), sino también por la curiosidad que me genera el tema de los autómatas celulares (AC) y la criptografía. En concreto se han desarrollado en Haskell algunos algoritmos de interés en los que se usarán autómatas celulares tanto para la generación de números pseudoaleatorios necesarios para RSA como para los procesos de cifrado y descifrado en el Cifrado de Wolfram y en el Cifrado de bloque basado en AC de segundo orden. La idea general es ahondar en el uso de los autómatas celulares dentro del ámbito de la Criptografía y comprobar si son una buena alternativa a otras que no los usan.


## Instalación

En primer lugar, es necesario descargar el proyecto en local para poder ser utilizado. Para ello, lo descargaremos como zip y lo guardaremos en una carpeta cualquiera (para evitar posibles problemas, se recomienda hacerlo en 'C:/').

![DescargaProyectoGitHub](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/60a76079-6b82-49e8-bced-cd40211a7da9) 

Hay varias formas de probar los algoritmos implementados en este proyecto. La más rápida y sencilla es usar un pequeño programa con el que podemos elegir qué algoritmo utilizar. Para ello, tenemos dos formas de hacerlo: mediante el Terminal o el IDE correspondiente (como este proyecto se ha desarrollado en Visual Studio Code, este pequeño tutorial se enfocará en este IDE en particular así como en el sistema operativo Windows). En ambos casos hay que escribir la misma línea (en el terminal, de antemano, hay que estar dentro de la carpeta del proyecto):


```
  cabal run algoritmos-criptograficos
```

## Utilización

Una vez que se ha ejecutado el comando, deberían aparecer varios mensajes, el último de ellos para continuar con la ejecución. Habría que elegir una de las tres opciones disponibles: RSA, Cifrado de bloque o Cifrado de Wolfram. Para que el programa siga funcionando correctamente, habrá que poner el nombre completo de los algoritmos o la versión "simplificada" que hay entre paréntesis: rsa, bloque o wolfram.

Para RSA habría que introducir, posteriormente, el mensaje a cifrar. Si todo sale bien, debería aparecer algo parecido a lo siguiente:

![Experimentos_RSA](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/cb822fc2-adbe-4bd2-9222-7061ba577e27)

Para la segunda opción (Cifrado de bloques), si elegimos la primera opción habría que introducir por teclado el mensaje a cifrar, el radio de vecindad para el autómata celular (1, 2 o 3) y la clave.

![Experimentos_Completo_Cifrado_Bloque](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/505c0982-0625-4b66-9566-2a0b9c80fbd0)

Si elegimos la segunda opción, solo habría que escribir el mensaje que queremos cifrar.

![Experimentos_Completo2_Cifrado_Bloque_Seguro](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/d1fda25c-2f74-443e-9747-6ff67e5c31d8)

Finalmente, para el Cifrado de Wolfram tendríamos que introducir el mensaje a cifrar y la clave secreta, que usaremos para la configuración inicial del autómata que será usado para generar la clave con la que se cifrará el mensaje.

![Experimentos_Wolfram](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/2cc8ce10-227b-4a4a-9152-29182c439d2b)

En caso de querer ejecutar por separado los autómatas celulares, habría que inicializar el entorno GHCI mediante `cabal repl`. Una vez iniciado, habría que cargar el fichero correspondiente: `:l src/AutomataCelular.hs` y ejecutar cualquiera de las siguientes funciones:

- creaYMuestraACelementalCondicionesFijas
- creaYMuestraACelementalCondicionesAleatorias
- creaACSegundoOrdenRadio2
- creaACSegundoOrdenRadio3

A continuación, se muestra una captura de la ejecución de la primera función con la regla 30.

![PruebaACRegla30CondicionesInicialesFijas-1celda](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/f8814bb4-96bc-4dc7-a6f3-973071f1efaf)

La siguiente captura, además, muestra parte de la ejecución del AC de segundo orden de radio 2 con la regla 30:

![Pruebas_ACSegundoOrdenRadio2_PrimeraParte-1](https://github.com/rcp-code/algoritmos-criptograficos-mediante-automatas-celulares/assets/56834727/2769332e-593e-430a-86a3-f9d97a143478)


## Futuras implementaciones

- Mejorar la eficiencia.
- Autómatas celulares bidimensionales.
- Criptosistema de Kari.
- Cifrado de imágenes mediante AC unidimensionales.
- Tests con QuickCheck.


## Créditos

Para la realización de este proyecto se han usado de manera total o parcial recursos de:

- [Rosetta Code](https://rosettacode.org/wiki/Elementary_cellular_automaton#Comonadic_solution)
- [Criptografía desde el punto de vista de la programación funcional - Daniel Rodríguez Chavarría](https://idus.us.es/bitstream/handle/11441/43818/Rodr%c3%adguez%20Chavarr%c3%ada%2c%20Daniel%20TFG.pdf?sequence=1&isAllowed=y)

## Licencia

[GPL-3.0](https://choosealicense.com/licenses/gpl-3.0/) 