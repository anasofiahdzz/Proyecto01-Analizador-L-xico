module MDD where

import AFD
import AFNEp
import AFN
import ER
import AFDmin
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Categoria = String
type Lexema = String

data Token
  = Token Categoria Lexema
  | Omitir
  | Error Lexema
  deriving (Show, Eq)

data MDD = MDD {
    afd :: AFD,
    mu :: Map.Map Estado Categoria
  } deriving (Show)

type DeltaMap = Map.Map (Estado, Char) Estado


lexer mdd input =
  let
    automata = afd mdd
    funcion_mu = mu mdd
    q0 = inicialD automata
    delta = Map.fromList [ ((q, c), dest) | (q, c, dest) <- transicionesD automata ]
  in scanner delta funcion_mu q0 input


scanner :: DeltaMap -> Map.Map Estado Categoria -> Estado -> String -> [Token]
scanner _ _ _ [] = []
scanner delta mu q0 input =
  let
    (lexema, restante, ultimo_final) = simular delta mu q0 input
  in
    case ultimo_final of
      
      Just (q_final, lexema_final) ->
        let categoria = mu Map.! q_final
            input_restante = drop (length lexema_final) input
            
            token = if categoria == "omitir"
                    then Omitir
                    else Token categoria lexema_final
                    
            tokens = if token == Omitir then [] else [token]

        in tokens ++ scanner delta mu q0 input_restante

      Nothing ->
        if null lexema
        then let (char_error, resto) = splitAt 1 input
             in [Error char_error] ++ scanner delta mu q0 resto
        else [Error lexema]


simular :: DeltaMap -> Map.Map Estado Categoria -> Estado -> String
        -> (Lexema, String, Maybe (Estado, Lexema))
simular delta mu q0 input =
  loop q0 "" input Nothing
  where
    loop q_actual lexema_actual input_restante ultimo_final_visto =
      
      let nuevo_ultimo_final =
            if Map.member q_actual mu
            then Just (q_actual, lexema_actual)
            else ultimo_final_visto
      in
        case input_restante of
          [] -> (lexema_actual, "", nuevo_ultimo_final)
          (c:cs) ->
            case Map.lookup (q_actual, c) delta of
              Just q_siguiente ->
                loop q_siguiente (lexema_actual ++ [c]) cs nuevo_ultimo_final
                
              Nothing ->
                (lexema_actual, input_restante, ultimo_final_visto)
