module Main where

import Lexer
import MDD (Token(..), lexer)
import System.IO (readFile)
import System.IO.Error (catchIOError, isDoesNotExistError)

main :: IO ()
main = do
  putStrLn "\n============================================="
  putStrLn "|| Construyendo Analizador Léxico para IMP ||"
  putStrLn "=============================================\n"
  putStrLn ">>> Construyendo Lista de MDDs en base a la gramática..."

  let listaMDDs = construirListaMDDs gramaticaIMP

  putStrLn ">>> ¡Listo! Leyendo la implementación a probar ..."

  codigoFuente <- leerFuente
  putStrLn "\n>>> Contenido leído:\n"
  putStrLn codigoFuente

  putStrLn "\n>>> Analizando entrada..."
  putStrLn "\n>>>> Tokens reconocidos <<<<"

  let tokens = lexer listaMDDs codigoFuente
      tokensVisibles = filter noEsOmitir tokens

  mapM_ print tokensVisibles

  putStrLn "\n>>> ¡Análisis léxico completado!\n"
  
  putStrLn "==========="
  putStrLn "|| Adios ||"
  putStrLn "===========\n"

leerFuente :: IO String
leerFuente = do
  let archivo = "main/implementacion.imp"
  res <- tryReadFile archivo
  case res of
    Just txt -> return txt
    Nothing  -> do
      putStrLn $ ">>> Advertencia: No se encontró el archivo " ++ archivo ++ ". Usando el ejemplo predeterminado."
      return ejemploIMP

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile path =
  catchIOError
    (do txt <- readFile path
        putStrLn $ ">>> Archivo leído: " ++ path
        return (Just txt))
    (\_ -> return Nothing)

noEsOmitir :: Token -> Bool
noEsOmitir Omitir = False
noEsOmitir _      = True

-- mismo ejemplo que tenías
ejemploIMP :: String
ejemploIMP =
  "x := 1;\n\
  \n := 5;\n\
  \while n <= 0 do (\n\
  \  factorial := factorial * n;\n\
  \  n := n - 1;\n\
  \);\n\
  \result := factorial;\n"

