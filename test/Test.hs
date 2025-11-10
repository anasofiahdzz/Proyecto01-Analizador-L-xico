-- test/Test.hs
module Main (main) where

-- Importa los módulos de prueba que vayas creando
import TestAFNEp

main :: IO ()
main = do
    putStrLn "\n\n   ############################################################"
    putStrLn "   ########## INICIANDO TESTS PARA ANALIZADOR LÉXICO ##########"
    putStrLn "   ############################################################\n"
    
    -- Llama a las pruebas de cada módulo
    runTestsAFNEp
    
    putStrLn "\n   #########################################"
    putStrLn "   ########## PRUEBAS FINALIZADAS ##########"
    putStrLn "   #########################################\n"
