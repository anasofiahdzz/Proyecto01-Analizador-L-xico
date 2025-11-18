# Cómo usar el Analizador Léxico IMP

La estructura que sigue este proyecto es la siguiente:  \

|-- docs/ \
|     | --  Reporte.pdf \
|     | --  Enteros.pdf \
| \
|-- main/ \
│    |-- Main.hs (Programa principal) \
│    |-- implementación.imp (Archivo de entrada para Main) \
| \
|-- src/ \
|    |-- AFD.hs (Lógica para pasar de AFN a AFD) \
|    |-- ADFmin.hs (Lógica para pasar de AFD a AFDmin) \
│    |-- AFN.hs (Lógica para pasar de AFNepsilon a AFN) \
|    |-- AFNEp.hs (Lógica para pasar de ER a AFNepsilon) \
|    |-- ER.hs (Lógica para construir una ER) \
|    |-- Gramatica.hs (Gramatica del lenguaje IMP) \
|    |-- Lexer.hs (Construcción de las MDD's) \
|    |-- MDD.hs (Lógica para construir una MDD con AFDmin) \
| \
|-- test/ \
|    |-- Test.hs (Módulo que ejecuta todas las pruebas de esta carpeta) \
|    |-- TestAFNEp.hs (Pruebas para AFNepsilon) \
|    |-- TestAFN.hs (Pruebas para AFN) \
|    |-- TestAFD.hs (Pruebas para AFD) \
|    |-- TestAFDmin.hs (Pruebas para AFD min) \
|    |-- TestMDD.hs (Pruebas para MDD) \
|    |-- TestLexer.hs (Pruebas finales del Lexer) \
| \
|-- package.yaml \
|-- Proyecto01-Analizador-L-xico.cabal \
|-- README.md (Este archivo) \
|-- stack.yaml \
\

La estrategia en general de toda la lógica es hacer varias MDDs en vez de solo una grandota. 

Para poder probar el proyecto se necesita tener instalado **Stack**. Lo primero que hay que hacer es estar en la carpeta Raiz, donde se encuentra 'stack.yaml' y 
'package.yaml'.

Para inicializar todo, podemos hacer
```
stack build
```

Luego, para probar el programa, debemos ejecutar 
```
stack run
```

Lo que el programa hará será analizar el contenido del archivo `implementacion.imp` que contiene el "código" que se analizará. Este archivo por
defecto ya tiene una implementación hecha por nosotros, pero se puede modificar como el usuario lo desee si sigue las reglas del lenguaje IMP para evitar errores
o analisis incorrectos. 

La salida del programa es la lista de todos los tokens encontrados en la implementación de entrada.

Para poder ejecutar las pruebas (Nos faltas eso), se tiene que ejecutar
```
stack test
```
