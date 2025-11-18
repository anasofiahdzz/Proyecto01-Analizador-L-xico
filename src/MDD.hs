-- >>>>> Definición de MDD y la función para pasar de AFDmin --> MDD <<<<<
module MDD where

import AFD
import AFNEp (Estado)

import qualified Data.Map.Strict as Mapa
-- minimumBy para la prioridad.
import Data.List (minimumBy)
-- comparing para usar con minimumBy.
import Data.Ord (comparing)

-- Categoría léxica. 
type Categoria = String
-- Texto del token.
type Lexema = String
-- Prioridad para no tener ambigüedades.
type Prioridad = Int

-- Definimos el tipo 'Token', que es la salida principal del lexer.
data Token
  -- Un token puede tener 3 casos:
  -- Token "clasico", con un lexema y su categoría.
  = Token Categoria Lexema
  -- Token que omite un lexema.
  | Omitir
  -- Token para reportar un error léxico.
  | Error Char
  deriving (Show, Eq)

-- Definimos las MDD, que son los componentes de un AFD y la función Mu.
data MDD = MDD {
    afd :: AFD,
    mu :: Mapa.Map Estado Categoria
  } deriving (Show)

-- Definimos el tipo MDDInfo para agrupar un MDD con su metadata.
data MDDInfo = MDDInfo {
    prioridad :: Prioridad,
    categoria :: Categoria,
    mdd :: MDD
} deriving (Show)



-- ###### Lógica de este módulo ######

-- Función que toma una lista de MDD, la cadena a analizar y regresa los tokens reconocidos.
lexer :: [MDDInfo] -> String -> [Token]
lexer listaMDDs input = go input []
  where
    -- 'go' sirve para ir consumiendo la entrada de forma recursiva.
    
    -- Si la cadena ya esta vacia, pues terminamos.
    go [] accTokens = reverse accTokens
    go currentInput@(c:cs) accTokens
      -- Manejar el final del archivo
      | c == '\0' = reverse accTokens
      | otherwise =
          -- Encontrar todos los matchs posibles desde la posición actual.
          let posiblesMatchs =
                -- Construimos una lista de tuplas (Prioridad, Categoría, Lexema).
                [ (prioridad info, categoria info, lexema)
                -- Iteramos sobre cada MDD definida en el lexer.
                | info <- listaMDDs
                -- Filtramos solo las que tuvieron éxito.
                , Just (lexema, _) <- [simular (mdd info) currentInput]
                -- Checar que el lexema reconocido no esté vacío.
                , not (null lexema)
                ]
          in
            -- Si ninguna MDD pudo reconocer nada, entonces se trata de un error léxico y regresamos el token de error.
            if null posiblesMatchs
              then go cs (Error c : accTokens)
              -- Una MDD tuvo éxito.
              else
                -- Encontramos la longitud máxima entre todos los lexemas de 'posiblesMatchs'.
                let maxLen = maximum [ length l | (_, _, l) <- posiblesMatchs ]
                    candidatos = [ (p, cat, lex) | (p, cat, lex) <- posiblesMatchs, length lex == maxLen ]
                    (mejorP, mejorCat, mejorLex) = minimumBy (comparing (\(p,_,_) -> p)) candidatos
                    resto = drop (length mejorLex) currentInput
                in
                  if mejorCat == "omitir"
                    then go resto accTokens
                    else go resto (Token mejorCat mejorLex : accTokens)
              
              
simular :: MDD -> String -> Maybe (Lexema, Estado)
simular mdd input =
  let
    deltaMap = Mapa.fromList [ ((q, c), dest) | (q, c, dest) <- transicionesD (afd mdd) ]
    q0 = inicialD (afd mdd)
    muMap = mu mdd

    ciclo :: Estado -> String -> String -> Maybe (Lexema, Estado) -> Maybe (Lexema, Estado)
    ciclo q consumido restante mejorMatchHastaAhora =
      let
        esFinalActual = Mapa.member q muMap
        nuevoMejorMatch = if esFinalActual && length consumido >= maybe 0 (length . fst) mejorMatchHastaAhora
                          then Just (consumido, q)
                          else mejorMatchHastaAhora
      in
        case restante of
          [] -> nuevoMejorMatch
          (c:cs) ->
            case Mapa.lookup (q, c) deltaMap of
              Just q_siguiente -> ciclo q_siguiente (consumido ++ [c]) cs nuevoMejorMatch
              Nothing -> nuevoMejorMatch

  in ciclo q0 "" input Nothing


procesarToken :: Categoria -> Lexema -> Token
procesarToken cat lexema = Token cat lexema
