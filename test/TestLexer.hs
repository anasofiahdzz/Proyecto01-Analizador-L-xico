module TestLexer (runTestsLexer) where

import Lexer (gramaticaIMP, construirListaMDDs)
import MDD (lexer, Token(..))

esVisible :: Token -> Bool
esVisible Omitir = False
esVisible _ = True

correrCaso :: String -> String -> [Token] -> IO ()
correrCaso nombre input tokens = do
    putStrLn $ "--> " ++ nombre
    putStrLn $ "    Entrada: " ++ show input
    putStrLn "    Salida (Tokens):"
    mapM_ (\t -> putStrLn $ "      " ++ show t) (filter esVisible tokens)
    putStrLn ""

runTestsLexer :: IO ()
runTestsLexer = do
    putStrLn "\n   =================================================="
    putStrLn "   ===== Pruebas Finales del Lexer (Integración) ===="
    putStrLn "   ==================================================\n"
    
    putStrLn ">>> Construyendo MDDs basados en gramaticaIMP..."
    let listaMDDs = construirListaMDDs gramaticaIMP
    putStrLn ">>> ¡MDDs construidas! Ejecutando casos...\n"

    let input1 = "contador := 10 + 50;"
    correrCaso "Caso 1: Asignación Simple" input1 (lexer listaMDDs input1)

    let input2 = "if true then if_variable := false"
    correrCaso "Caso 2: Prioridad (Keyword vs ID)" input2 (lexer listaMDDs input2)

    let input3 = "x := 1; // Inicializacion \n y := 2;"
    correrCaso "Caso 3: Filtrado de Comentarios" input3 (lexer listaMDDs input3)

    let input4 = "while n <= 100 do n := n * 2"
    correrCaso "Caso 4: Maximal Munch (<= vs <)" input4 (lexer listaMDDs input4)

    let input5 = "val := 10 # 20 ?;"
    correrCaso "Caso 5: Caracteres Inválidos" input5 (lexer listaMDDs input5)

    putStrLn "\n   ================================"
    putStrLn "   === Fin de las pruebas Lexer ==="
    putStrLn "   ================================\n"
