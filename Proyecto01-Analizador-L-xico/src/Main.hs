module Main where

import ER
import Gramatica hiding (alfabeto)
import AFNEp
import AFN
import AFD
import AFDmin (minimizaAFD)
import MDD

import qualified Data.Map.Strict as Map
import Data.List (nub, foldl', minimumBy)
import Data.Ord (comparing)
import System.IO (readFile)
import Control.Exception (catch)
import System.IO.Error
  ( IOError
  , ioError, userError
  , catchIOError, tryIOError
  , isDoesNotExistError, isPermissionError, isEOFError
  )

type GramaticaLexica = [(Categoria, Prioridad, Expr)]
type Prioridad = Int

gramaticaIMP :: GramaticaLexica
gramaticaIMP =
  [
    ("PALABRA_RESERVADA", 1, palabrasReservadas),
    ("IDENTIFICADOR",     2, identificador),
    ("ENTERO",            3, enteros),
    ("OPERADOR",          4, operadores),
    ("DELIMITADOR",       5, delimitadores),
    ("omitir",           10, espacios),
    ("omitir",           10, comentarios)
  ]

prioridadTokens :: Map.Map Categoria Prioridad
prioridadTokens = Map.fromList [ (cat, pri) | (cat, pri, _) <- gramaticaIMP ]

renombrarAFNEp :: Categoria -> AFNEp -> AFNEp
renombrarAFNEp prefijo afnep =
  let
    renombrar q = prefijo ++ "_" ++ q
  in
    afnep {
      estados = map renombrar (estados afnep),
      inicial = renombrar (inicial afnep),
      final = renombrar (final afnep),
      transiciones = [ (renombrar q, c, map renombrar qs)
                     | (q, c, qs) <- transiciones afnep
                     ]
    }


construirMuFinal :: AFN -> [([Estado], Estado)] -> Map.Map Estado Categoria -> Map.Map Estado Categoria 
construirMuFinal afnCombo todosEstados muIntermedio =
  let
    finalesAFD = calcularFinales afnCombo todosEstados
    paresMuFinal =
      [ (nombreAFD, categoriaGanadora)
      | (conjuntoAFN, nombreAFD) <- todosEstados, nombreAFD `elem` finalesAFD
      , let finalesOriginales = filter (`Map.member` muIntermedio) conjuntoAFN
      , not (null finalesOriginales)
      , let candidatos = [ (prioridadTokens Map.! (muIntermedio Map.! q), muIntermedio Map.! q)
                         | q <- finalesOriginales
                         ]
      , let (priMin, categoriaGanadora) = minimumBy (comparing fst) candidatos
      ]
  in Map.fromList paresMuFinal


construirMDD :: GramaticaLexica -> MDD
construirMDD gramatica =
    
    let
      afnEpsRenombrados =
        [ (cat, renombrarAFNEp cat (compilarAFNEp expr))
        | (cat, _, expr) <- gramatica
        ]

      muIntermedio = Map.fromList
        [ (final afnep, cat)
        | (cat, afnep) <- afnEpsRenombrados
        ]
        
      q_inicio = "q_MDD_INICIO"
      q_final  = "q_MDD_FINAL"
      
      todosEstados = q_inicio : q_final : concatMap (estados . snd) afnEpsRenombrados
      
      todoAlfabeto = nub $ concatMap (alfabeto . snd) afnEpsRenombrados

      transSimbolo = concatMap (transiciones . snd) afnEpsRenombrados
      
      transEpsInicio = (q_inicio, Nothing, map (inicial . snd) afnEpsRenombrados)
      
      transEpsFinales = [ (final_original, Nothing, [q_final])
                        | (cat, afnep) <- afnEpsRenombrados
                        , let final_original = final afnep
                        ]
                        
      todasTrans = transEpsInicio : transEpsFinales ++ transSimbolo
      
      afnepCombinado = AFNEp todosEstados todoAlfabeto todasTrans q_inicio q_final

      afnCombinado = aFNEp_to_AFN afnepCombinado
    
      afdCombinado = aFN_to_AFD afnCombinado

      afdMinimizado = minimizaAFD afdCombinado

      todosEstadosPreMinim = descubrirEstados afnCombinado

      muFinalPreMinim = construirMuFinal afnCombinado todosEstadosPreMinim muIntermedio
    
      muFinal = muFinalPreMinim
      afdFinal = afdCombinado

      in MDD afdFinal muFinal

main :: IO ()
main = do
    putStrLn "================================================"
    putStrLn "Construyendo Analizador Léxico para IMP..."
    putStrLn "================================================"

    putStrLn "[PASO 1/3] Construyendo MDD desde Gramatica.hs..."
    let mddIMP = construirMDD gramaticaIMP
    
    putStrLn "[PASO 2/3] MDD construida. Leyendo archivo 'test.imp'..."
    
    let archivoPrueba = "test.imp"
    -- if x := 5 then 
    --   skip // Esto es un comentario
    -- else
    --   y := 1
    --
    -- 9..12 // error léxico
    codigoFuente <- tryReadFile archivoPrueba
    
    putStrLn $ "Contenido de '" ++ archivoPrueba ++ "':\n" ++ codigoFuente
    putStrLn "[PASO 3/3] Ejecutando lexer..."
    putStrLn "================================================"
    putStrLn "Tokens Reconocidos:"
    putStrLn "================================================"

    let tokens = lexer mddIMP (codigoFuente ++ "\0")

    let tokensVisibles = filter noEsOmitir tokens
    mapM_ print tokensVisibles
    
    putStrLn "================================================"
    putStrLn "¡Análisis léxico completado!"
    putStrLn "================================================"

noEsOmitir :: Token -> Bool
noEsOmitir Omitir = False
noEsOmitir _      = True

tryReadFile :: FilePath -> IO String
tryReadFile path =
  catch (readFile path) handleErr
  where
    handleErr :: IOError -> IO String
    handleErr e = do
      putStrLn $ "ADVERTENCIA: No se pudo leer el archivo '" ++ path ++ "'."
      putStrLn "Usando código de ejemplo hardcodeado."
      return "if x := 5 then \n  skip // Esto es un comentario\nelse\n  y := 1\n\n9..12 // error lexico"
