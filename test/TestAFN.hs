module TestAFN (runTestsAFN) where

import ER
import AFNEp
import AFN
import Gramatica

er1 :: Expr
er1 = And (Term '0') (Kleene (Or (Term '1') (Term '0')))

er2 :: Expr
er2 = And (Kleene (Or (And (Term 'a') (Term 'b')) (Term 'b'))) (And (Term 'a') (Kleene (Term 'b')))

er3 :: Expr
er3 = enteros

runTestsAFN :: IO ()
runTestsAFN = do
    putStrLn "\n   ================================================="
    putStrLn "   ===== Pruebas del módulo AFN (AFNEp -> AFN) ====="
    putStrLn "   =================================================\n"
    
    putStrLn "----> Prueba 1 : ' 0(1+0)^* ' <-----"
    let afnEp1 = compilarAFNEp er1
    let afn1 = aFNEp_to_AFN afnEp1
    print afn1
    
    putStrLn "\n----> Prueba 2 : ' (ab + b)^* ab^* ' <-----"
    let afnEp2 = compilarAFNEp er2
    let afn2 = aFNEp_to_AFN afnEp2
    print afn2
    
    putStrLn "\n----> Prueba 3 : ' 0 + [1-9][0-9]^* + -[1-9][0-9]^* ' <-----"
    let afnEp3 = compilarAFNEp er3
    let afn3 = aFNEp_to_AFN afnEp3
    print afn3

    putStrLn "\n   =============================="
    putStrLn "   === Fin de las pruebas AFN ==="
    putStrLn "   ==============================\n"
