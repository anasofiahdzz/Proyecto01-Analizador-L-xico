module TestAFDmin (runTestsAFDmin) where

import ER
import AFNEp
import AFN
import AFD
import AFDmin
import Gramatica

er1 :: Expr
er1 = And (Term '0') (Kleene (Or (Term '1') (Term '0')))

er2 :: Expr
er2 = And (Kleene (Or (And (Term 'a') (Term 'b')) (Term 'b'))) (And (Term 'a') (Kleene (Term 'b')))

er3 :: Expr
er3 = enteros

runTestsAFDmin :: IO ()
runTestsAFDmin = do
    putStrLn "\n   ====================================================="
    putStrLn "   ===== Pruebas del módulo AFDmin (AFD -> AFDmin) ====="
    putStrLn "   =====================================================\n"
    
    putStrLn "----> Prueba 1 : ' 0(1+0)^* ' <-----"
    let afnEp1 = compilarAFNEp er1
    let afn1 = aFNEp_to_AFN afnEp1
    let afd1 = aFN_to_AFD afn1
    let afdMin1 = minimizaAFD afd1
    print afdMin1
    
    putStrLn "\n----> Prueba 2 : ' (ab + b)^* ab^* ' <-----"
    let afnEp2 = compilarAFNEp er2
    let afn2 = aFNEp_to_AFN afnEp2
    let afd2 = aFN_to_AFD afn2
    let afdMin2 = minimizaAFD afd2
    print afdMin2
    
    putStrLn "\n----> Prueba 3 : ' 0 + [1-9][0-9]^* + -[1-9][0-9]^* ' <-----"
    let afnEp3 = compilarAFNEp er3
    let afn3 = aFNEp_to_AFN afnEp3
    let afd3 = aFN_to_AFD afn3
    let afdMin3 = minimizaAFD afd3
    print afdMin3

    putStrLn "\n   =================================="
    putStrLn "   === Fin de las pruebas AFDmin ===="
    putStrLn "   ==================================\n"
