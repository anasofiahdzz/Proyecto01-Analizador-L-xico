module Main (main) where

import TestAFNEp
import TestAFN
import TestAFD
import TestAFDmin
import TestMDD
import TestLexer

main :: IO ()
main = do
    putStrLn "\n\n   ############################################################"
    putStrLn "   ########## INICIANDO TESTS PARA ANALIZADOR LÉXICO ##########"
    putStrLn "   ############################################################\n"
    
    runTestsAFNEp
    runTestsAFN
    runTestsAFD
    runTestsAFDmin
    runTestsMDD
    runTestsLexer
    
    putStrLn "\n   #########################################"
    putStrLn "   ########## PRUEBAS FINALIZADAS ##########"
    putStrLn "   #########################################\n"
