module MDD where

import AFD
import AFNEp (Estado)

import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)

type Categoria = String
type Lexema = String
type Prioridad = Int


data Token
  = Token Categoria Lexema
  | Omitir
  | Error Char
  deriving (Show, Eq)

data MDD = MDD {
    afd :: AFD,
    mu :: Map.Map Estado Categoria
  } deriving (Show)

data MDDInfo = MDDInfo {
    prioridad :: Prioridad,
    categoria :: Categoria,
    mdd :: MDD
} deriving (Show)



lexer :: [MDDInfo] -> String -> [Token]
lexer listaMDDs input = go input []
  where
    go [] accTokens = reverse accTokens
    go currentInput@(c:cs) accTokens
      | c == '\0' = reverse accTokens      -- 👈 detenerse sin error
      | otherwise =
          let posiblesMatches =
                [ (prioridad info, categoria info, lexema)
                | info <- listaMDDs
                , Just (lexema, _) <- [simular (mdd info) currentInput]
                , not (null lexema)
                ]
          in
            if null posiblesMatches
              then go cs (Error c : accTokens)
              else
                let maxLen = maximum [ length l | (_, _, l) <- posiblesMatches ]
                    candidatos = [ (p, cat, lex) | (p, cat, lex) <- posiblesMatches, length lex == maxLen ]
                    (mejorP, mejorCat, mejorLex) = minimumBy (comparing (\(p,_,_) -> p)) candidatos
                    resto = drop (length mejorLex) currentInput
                in
                  if mejorCat == "omitir"
                    then go resto accTokens
                    else go resto (Token mejorCat mejorLex : accTokens)
              
              
simular :: MDD -> String -> Maybe (Lexema, Estado)
simular mdd input =
  let
    deltaMap = Map.fromList [ ((q, c), dest) | (q, c, dest) <- transicionesD (afd mdd) ]
    q0 = inicialD (afd mdd)
    muMap = mu mdd

    loop :: Estado -> String -> String -> Maybe (Lexema, Estado) -> Maybe (Lexema, Estado)
    loop q consumido restante mejorMatchHastaAhora =
      let
        esFinalActual = Map.member q muMap
        nuevoMejorMatch = if esFinalActual && length consumido >= maybe 0 (length . fst) mejorMatchHastaAhora
                          then Just (consumido, q)
                          else mejorMatchHastaAhora
      in
        case restante of
          [] -> nuevoMejorMatch
          (c:cs) ->
            case Map.lookup (q, c) deltaMap of
              Just q_siguiente -> loop q_siguiente (consumido ++ [c]) cs nuevoMejorMatch
              Nothing -> nuevoMejorMatch

  in loop q0 "" input Nothing


procesarToken :: Categoria -> Lexema -> Token
procesarToken cat lexema = Token cat lexema
