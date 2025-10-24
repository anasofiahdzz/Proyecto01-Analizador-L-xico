-- test/Test.hs
module Main where

-- YA NO import Main  ← esto era el problema

import MDD
import Lexer

import Control.Monad (unless)
import System.Exit (exitFailure)

-- tipo de token y MDDInfo deben salir de MDD; si tu MDD no los exporta, expórtalos.
-- ajusta imports según tu MDD.hs

-- -------------------------------
-- Casos de prueba
-- -------------------------------
type CasoPrueba = (String, [Token])

casosDePrueba :: [CasoPrueba]
casosDePrueba =
  [ ( "if x := 5 then skip else y := 1"
    , [ Token "PALABRA_RESERVADA" "if"
      , Token "IDENTIFICADOR" "x"
      , Token "OPERADOR" ":="
      , Token "ENTERO" "5"
      , Token "PALABRA_RESERVADA" "then"
      , Token "PALABRA_RESERVADA" "skip"
      , Token "PALABRA_RESERVADA" "else"
      , Token "IDENTIFICADOR" "y"
      , Token "OPERADOR" ":="
      , Token "ENTERO" "1"
      ]
    )
  , ( "ifx := if"
    , [ Token "IDENTIFICADOR" "ifx"
      , Token "OPERADOR" ":="
      , Token "PALABRA_RESERVADA" "if"
      ]
    )
  , ( "x := $ 5"
    , [ Token "IDENTIFICADOR" "x"
      , Token "OPERADOR" ":="
      , Error '$'
      , Token "ENTERO" "5"
      ]
    )
  , ( "" , [] )
  ]

-- filtra Omitir
filtrarOmitir :: [Token] -> [Token]
filtrarOmitir = filter (\t -> case t of Omitir -> False; _ -> True)

main :: IO ()
main = do
  putStrLn "== Tests del lexer =="
  let mdds = construirListaMDDs gramaticaIMP
  resultados <- mapM (runCase mdds) (zip [1..] casosDePrueba)
  let fallos = length (filter not resultados)
  unless (fallos == 0) exitFailure

runCase :: [MDDInfo] -> (Int, CasoPrueba) -> IO Bool
runCase mdds (n, (input, esperado)) = do
  let obtenido = filtrarOmitir (lexer mdds input)
  if obtenido == esperado
    then do
      putStrLn $ "Test #" ++ show n ++ " OK"
      pure True
    else do
      putStrLn $ "Test #" ++ show n ++ " FALLÓ"
      putStrLn $ "  entrada:   " ++ show input
      putStrLn $ "  esperado:  " ++ show esperado
      putStrLn $ "  obtenido:  " ++ show obtenido
      pure False

