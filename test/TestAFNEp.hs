-- Pruebas para AFNEp
module TestAFNEp (runTestsAFNEp) where

import ER
import AFNEp
import Gramatica

-- Vamos a probar 3 ER sencillas:

-- (1) -->  0(1 + 0)^*
er1 :: Expr
er1 = And (Term '0') (Kleene (Or (Term '1') (Term '0')))

-- (2) --> (ab + b)^∗ ab^∗
er2 :: Expr
er2 = And (Kleene (Or (And (Term 'a') (Term 'b')) (Term 'b'))) (And (Term 'a') (Kleene (Term 'b')))

-- (3) --> ER de los enteros 
er3 :: Expr
er3 = enteros

-- Una función principal para correr las pruebas de este módulo
runTestsAFNEp :: IO ()
runTestsAFNEp = do
    putStrLn "\n   =============================================="
    putStrLn "   === Pruebas del módulo AFNEp (ER -> AFNEp) ==="
    putStrLn "   ==============================================\n"
    
    putStrLn "----> Prueba 1 : ' 0(1+0)^* ' <-----"
    let afnEp1= compilarAFNEp er1
    print afnEp1
    
    putStrLn "\n----> Prueba 2 : ' (ab + b)^* ab^* ' <-----"
    let afnEp2= compilarAFNEp er2
    print afnEp2
    
    putStrLn "\n----> Prueba 3 : ' 0 + [1-9][0-9]^* + -[1-9][0-9]^* ' <-----"
    let afnEp3= compilarAFNEp er3
    print afnEp3


    putStrLn "\n   ================================"
    putStrLn "   === Fin de las pruebas AFNEp ==="
    putStrLn "   ================================\n"
