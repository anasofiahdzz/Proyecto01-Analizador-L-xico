module Main where

import ER
import AFNEp
import AFN
import AFD

-- Ejemplo de expresiones regulares:
-- 1. (a+b)*c  → cualquier secuencia de a's y b's seguida de una c
-- 2. [a-z]     → cualquier letra minúscula
-- 3. a(b|c)    → una 'a' seguida de 'b' o 'c'

main :: IO ()
main = do
    putStrLn "=== Prueba de conversion ER -> AFNε ==="
    
    -- Ejemplo 1: (a+b)*c
    let expr1 = And (Kleene (Or (Term 'a') (Term 'b'))) (Term 'c')
    let (afnEps1, _) = expr_to_AFNEp expr1 0
    let afn1 = afnEp_to_AFN afnEps1
    let afd1 = afn_to_afd afn1
    putStrLn "\nExpresion regular: (a+b)*c"
    print expr1
    putStrLn "\nAFNε resultante:"
    printAFNEp afnEps1
    putStrLn "\n AFN (sin ε):"
    printAFN afn1
    putStrLn "\n AFD (determinista):"
    printAFD afd1

    -- Ejemplo 2: [a-z]
    let expr2 = Range 'a' 'z'
    let (afnEps2, _) = expr_to_AFNEp expr2 0
    let afn2 = afnEp_to_AFN afnEps2
    let afd2 = afn_to_afd afn2
    putStrLn "\nExpresion regular: [a-z]"
    print expr2
    putStrLn "\nAFNε resultante:"
    printAFNEp afnEps2
    putStrLn "\n AFN (sin ε):"
    printAFN afn2
    putStrLn "\n AFD (determinista):"
    printAFD afd2

    -- Ejemplo 3: a(b|c)
    let expr3 = And (Term 'a') (Or (Term 'b') (Term 'c'))
    let (afnEps3, _) = expr_to_AFNEp expr3 0
    let afn3 = afnEp_to_AFN afnEps3
    let afd3 = afn_to_afd afn3
    putStrLn "\nExpresion regular: a(b|c)"
    print expr3
    putStrLn "\nAFNε resultante:"
    printAFNEp afnEps3
    putStrLn "\n AFN (sin ε):"
    printAFN afn3
    putStrLn "\n AFD (determinista):"
    printAFD afd3

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

printAFN :: AFN -> IO ()
printAFN m = do
    putStrLn $ "Estados: " ++ show (estadosN m)
    putStrLn $ "Alfabeto: " ++ show (alfabetoN m)
    putStrLn "Transiciones:"
    mapM_ printTrans (transicionesN m)
    putStrLn $ "Estado inicial: " ++ inicialN m
    putStrLn $ "Estados finales: " ++ show (finalesN m)
  where
    printTrans (q, c, qs) = putStrLn $ "  " ++ q ++ " --" ++ [c] ++ "--> " ++ show qs

printAFD :: AFD -> IO ()
printAFD m = do
    putStrLn $ "Estados: " ++ show (estadosD m)
    putStrLn $ "Alfabeto: " ++ show (alfabetoD m)
    putStrLn "Transiciones:"
    mapM_ printTrans (transicionesD m)
    putStrLn $ "Estado inicial: " ++ inicialD m
    putStrLn $ "Estados finales: " ++ show (finalesD m)
  where
    printTrans (q, c, q2) = putStrLn $ "  " ++ q ++ " --" ++ [c] ++ "--> " ++ q2


