module Main where

import ER
import AFNEp

-- Ejemplo de expresiones regulares:
-- 1. (a+b)*c  → cualquier secuencia de a's y b's seguida de una c
-- 2. [a-z]     → cualquier letra minúscula
-- 3. a(b|c)    → una 'a' seguida de 'b' o 'c'

main :: IO ()
main = do
    putStrLn "=== Prueba de conversion ER -> AFNε ==="
    
    -- Ejemplo 1: (a+b)*c
    let expr1 = And (Kleene (Or (Term 'a') (Term 'b'))) (Term 'c')
    let (afn1, _) = expr_to_AFNEp expr1 0
    putStrLn "\nExpresion regular: (a+b)*c"
    print expr1
    putStrLn "\nAFNε resultante:"
    printAFNEp afn1

    -- Ejemplo 2: [a-z]
    let expr2 = Range 'a' 'z'
    let (afn2, _) = expr_to_AFNEp expr2 0
    putStrLn "\nExpresion regular: [a-z]"
    print expr2
    putStrLn "\nAFNε resultante:"
    printAFNEp afn2

    -- Ejemplo 3: a(b|c)
    let expr3 = And (Term 'a') (Or (Term 'b') (Term 'c'))
    let (afn3, _) = expr_to_AFNEp expr3 0
    putStrLn "\nExpresion regular: a(b|c)"
    print expr3
    putStrLn "\nAFNε resultante:"
    printAFNEp afn3

-- Función auxiliar para imprimir bonito el AFNε
printAFNEp :: AFNEp -> IO ()
printAFNEp m = do
    putStrLn $ "Estados: " ++ show (estados m)
    putStrLn $ "Alfabeto: " ++ show (alfabeto m)
    putStrLn "Transiciones:"
    mapM_ printTrans (transiciones m)
    putStrLn $ "Estado inicial: " ++ inicial m
    putStrLn $ "Estado final: " ++ final m
  where
    printTrans (q, Nothing, qs) = putStrLn $ "  " ++ q ++ " --ε--> " ++ show qs
    printTrans (q, Just c, qs)  = putStrLn $ "  " ++ q ++ " --" ++ [c] ++ "--> " ++ show qs
