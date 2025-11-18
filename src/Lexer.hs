module Lexer (GramaticaLexica, Prioridad, gramaticaIMP, construirMDDIndividual, construirListaMDDs) where

import ER
import Gramatica
import AFNEp
import AFN
import AFD
import AFDmin
import MDD hiding (Prioridad)
import qualified Data.Map.Strict as Mapa

type GramaticaLexica = [(Categoria, Prioridad, Expr)]
type Prioridad = Int

gramaticaIMP :: GramaticaLexica
gramaticaIMP =
  [ ("PALABRA_RESERVADA", 1, palabrasReservadas)
  , ("IDENTIFICADOR", 2, identificador)
  , ("ENTERO", 3, enteros)
  , ("OPERADOR", 4, operadores)
  , ("DELIMITADOR", 5, delimitadores)
  , ("omitir", 10, espacios)
  , ("omitir", 10, comentarios)
  ]

construirMDDIndividual :: Categoria -> Expr -> MDD
construirMDDIndividual categoria expr =
  let afnEps  = compilarAFNEp expr
      afn     = aFNEp_to_AFN afnEps
      afd     = aFN_to_AFD afn
      afdMin  = minimizaAFD afd
      muSimple = Mapa.fromList [ (estadoFinal, categoria) | estadoFinal <- finalesD afdMin ]
  in MDD afdMin muSimple

construirListaMDDs :: GramaticaLexica -> [MDDInfo]
construirListaMDDs gr =
  [ MDDInfo { prioridad = prio, categoria = cat, mdd = construirMDDIndividual cat expr }
  | (cat, prio, expr) <- gr
  ]

