module TestMDD (runTestsMDD) where

import qualified Data.Map.Strict as Mapa
import Data.List (intercalate)

-- Importamos todos los módulos del pipeline
import ER
import AFNEp
import AFN
import AFD
import AFDmin
import MDD
import Gramatica -- Importamos las ER definidas para IMP

-- === Función Auxiliar: El Pipeline Completo ===
-- Esta función toma una ER, una categoría (String) y una prioridad (Int).
-- Realiza todas las transformaciones y devuelve un MDDInfo listo para el lexer.
construirMDD :: Expr -> String -> Int -> MDDInfo
construirMDD er nombreCategoria prioridad =
    let 
        -- 1. ER -> AFNepsilon
        afnep = compilarAFNEp er
        -- 2. AFNepsilon -> AFN
        afn = aFNEp_to_AFN afnep
        -- 3. AFN -> AFD
        afd = aFN_to_AFD afn
        -- 4. AFD -> AFDmin
        afdMin = minimizaAFD afd
        
        -- 5. Construcción de la función mu (μ)
        -- En nuestra estrategia de "MDDs pequeñas", todos los estados finales 
        -- de este autómata apuntan a la MISMA categoría.
        estadosFinales = finalesD afdMin
        funcionMu = Mapa.fromList [ (q, nombreCategoria) | q <- estadosFinales ]
        
        -- 6. Crear la estructura MDD
        miMDD = MDD { afd = afdMin, mu = funcionMu }
        
    in MDDInfo { 
        prioridad = prioridad, 
        categoria = nombreCategoria, 
        mdd = miMDD 
    }

-- === Configuración del Lexer ===
-- Creamos la lista de MDDs que usará el lexer.
-- Recuerda: Menor número = Mayor prioridad.
listaDeMDDs :: [MDDInfo]
listaDeMDDs = [
    -- Prioridad 0: Lo más importante (Espacios y Palabras Reservadas)
    -- "omitir" es una palabra clave especial en tu MDD.hs para ignorar el token.
    construirMDD espacios            "omitir"             0,
    construirMDD palabrasReservadas  "PalabraReservada"   1, 
    construirMDD delimitadores       "Delimitador"        1,
    construirMDD operadores          "Operador"           1,
    
    -- Prioridad 2: Identificadores (Si es "if", gana PalabrasReservadas por prioridad)
    construirMDD identificador       "Identificador"      2,
    
    -- Prioridad 3: Literales
    construirMDD enteros             "Entero"             3
    ]

-- === Pruebas ===

runTestsMDD :: IO ()
runTestsMDD = do
    putStrLn "\n   ============================================="
    putStrLn "   ===== Pruebas del módulo MDD (Lexer) ======="
    putStrLn "   =============================================\n"

    -- Caso 1: Asignación simple
    let input1 = "x := 10 + 20;"
    putStrLn $ "Entrada: " ++ show input1
    putStrLn "Tokens:"
    printTokens (lexer listaDeMDDs input1)
    
    putStrLn "\n---------------------------------------------\n"

    -- Caso 2: Estructura de control (prueba prioridad Keyword vs ID)
    -- 'if' y 'then' deberían ser PalabraReservada, no Identificador.
    let input2 = "if true then x := 0 else skip"
    putStrLn $ "Entrada: " ++ show input2
    putStrLn "Tokens:"
    printTokens (lexer listaDeMDDs input2)

    putStrLn "\n---------------------------------------------\n"

    -- Caso 3: Manejo de errores
    -- El símbolo '?' no está en nuestra gramática
    let input3 = "x := ?"
    putStrLn $ "Entrada: " ++ show input3
    putStrLn "Tokens (Debería mostrar un Error):"
    printTokens (lexer listaDeMDDs input3)

    putStrLn "\n   =============================="
    putStrLn "   === Fin de las pruebas MDD ==="
    putStrLn "   ==============================\n"

-- Función auxiliar para imprimir bonito
printTokens :: [Token] -> IO ()
printTokens tokens = mapM_ print tokens
