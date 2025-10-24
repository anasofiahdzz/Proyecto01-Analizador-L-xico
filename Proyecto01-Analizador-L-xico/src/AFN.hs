-- >>>>> Definición de AFN y la función para pasar de AFNepsilon --> AFN <<<<<
module AFN (AFN(..), Trans_afn, aFNEp_to_AFN) where

-- Se importa la definición de AFNEp.
import AFNEp
-- Importamos nub para eliminar duplicados
import Data.List (nub)
import qualified Data.Set as S


-- Definición de una transición con un símbolo.
-- Estado origen -> Símbolo -> Lista de estados destino
type Trans_afn = (Estado, Char, [Estado]) -- (estadoDeOrigen, simboloLeido, listaDeEstadosDestino)


-- Definición de Autómata no determinista
data AFN = AFN {
    estados2 ::[Estado], -- Lista de todos los estados.
    alfabeto2 :: [Char], -- Alfabeto que acepta el autómata.
    transiciones2 :: [Trans_afn], -- Reglas de transición.
    inicial2 :: Estado, -- Estado inicial.
    finales2 :: [Estado]-- Estados finales.
  } deriving (Eq)
  
-- Show personalizado, para poder imprimir mejor nuestra autómata.
instance Show AFN where
  show m =
    unlines
      [ "AFN {"
      , "  estados      = " ++ show (estados2 m)
      , "  alfabeto     = " ++ show (alfabeto2 m)
      , "  transiciones = " ++ show (transiciones2 m)
      , "  inicial      = " ++ show (inicial2 m)
      , "  finales      = " ++ show (finales2 m)
      , "}"
      ]

-- ========== Lógica para pasar de AFNEp a AFN ========== 

eclosure :: AFNEp -> Estado -> [Estado]
eclosure m q =
    -- S.toList convierte el Set final de estados en una lista
    S.toList (go [q] S.empty)
  where
    -- 'go' implementa una búsqueda (DFS)
    -- worklist: Estados que nos faltan por revisar
    -- visitados: Set de estados que ya hemos procesado
    go :: [Estado] -> S.Set Estado -> S.Set Estado
    go [] visitados = visitados -- Caso base: no hay más trabajo
    go (actual:resto_worklist) visitados
      
      -- Si ya visitamos este estado, lo ignoramos y seguimos
      | actual `S.member` visitados = go resto_worklist visitados
      
      -- Si es un estado nuevo:
      | otherwise =
          -- 1. Lo marcamos como visitado
          let nuevosVisitados = S.insert actual visitados
          -- 2. Encontramos todos sus vecinos épsilon directos
              vecinosEps = concat [ dest | (p, Nothing, dest) <- transiciones m, p == actual ]
          -- 3. Seguimos la búsqueda, añadiendo los nuevos vecinos
          --    al frente de la lista de trabajo (DFS)
          in go (vecinosEps ++ resto_worklist) nuevosVisitados

-- | Función auxiliar que calcula la e-closure para CADA estado en una lista.
eclosureSet :: AFNEp -> [Estado] -> [Estado]
eclosureSet m qs = nub (concatMap (eclosure m) qs)

-- (2) Ahora calculamos la nueva transición para un conjunto de estados y un símbolo.

-- DUDOTA
do_trans :: AFNEp -> [Estado] -> Char -> [Estado]
do_trans m listaEstados a =
  nub $ concat [ destList | (q, Just c, destList) <- transiciones m, -- Para cada transición con símbolo.
                            q `elem` listaEstados,      -- que empiece en un estado de nuestro conjunto 'setS'
                            c == a ]            -- y que use el símbolo 'a'
                            
-- (3) d'(q, a) = EC ( d ( EC (q) , a ) )                        


-- Generamos la lista de todas las transiciones para el nuevo AFN.
transicionesAFN :: AFNEp -> [Trans_afn]
transicionesAFN m =
  -- Para cada estado q en el autómata
  concatMap (transicionesAFN_aux m (alfabeto m)) (estados m)

-- Calculamos todas las transiciones desde un estado q para cada símbolo, a partir del autómata con epsilom,
-- sus transiciones, el alfabeto y un estado q.
transicionesAFN_aux :: AFNEp -> [Char] -> Estado -> [Trans_afn]
transicionesAFN_aux _ [] _ = [] -- Alfabeto vació, que es el caso base.
transicionesAFN_aux m (x:xs) q =
  -- Construimos la transición para el símbolo x y se sigue con el resto.
  (q, x, qn) : transicionesAFN_aux m xs q
  where
    -- cq -> EC(q)
    cq = eclosure m q
    -- paso -> d (cq, c) 
    paso = do_trans m cq x
    -- qn -> EC(paso)
    qn = eclosureSet m paso


-- (4) Calculamos los nuevos estados finales para AFN


nuevosFinales :: AFNEp -> [Estado]
-- Un estado es final si su E-closure contenía al estado final del AFNEp.
nuevosFinales m = [ q | q <- estados m, final m `elem` eclosure m q ]


-- (5) Ensamblamos todo 
aFNEp_to_AFN :: AFNEp -> AFN
aFNEp_to_AFN m = AFN {estados2 = estados m, alfabeto2 = alfabeto m,
                    transiciones2 = transicionesAFN m,
                    inicial2 = inicial m, finales2 = nuevosFinales m
                    }
