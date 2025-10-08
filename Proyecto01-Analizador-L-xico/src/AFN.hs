module AFN where
import Data.Set (toList, fromList)
import ER          
import AFNEp

-- Transición no determinista (SIN epsilon)
type Trans_afn = (String, Char, [String]) -- (estadoDeOrigen, simboloLeido, listaDeEstadosDestino)

-- Definición de Automata no determinista (SIN epsilon)
data AFN = AFN{ estadosN ::[String], alfabetoN :: [Char],
                transicionesN :: [Trans_afn],
                inicialN :: String, finalesN :: [String]-- Estados que eClosure contiene al final del AFNε
  } deriving (Show)
--Nos aseguraramos de que ningun estado se repita en las listas,aqui se eliminan los estados repetidos y se ordenan
rmDup :: (Ord a) => [a] -> [a]
rmDup = toList . fromList --fromList quita duplicados al convertir la lista en un Set
                         --toList vuelve a convertir el Set en lista
--Calculamos las e-closures de un estado q1
eclosure :: [Trans_eps] -> AFNEp -> String -> [String]
eclosure transiciones m q1 = rmDup (q1 : eclosure2 m epsNext) --toma q1, busca adónde llega por epsilon y repite el proceso
  where
    --es la lista de estados a los que llegamos por epsilon desde q1.
    epsNext = concat [ destino | (p, Nothing, destino) <- transiciones, p == q1 ] 

eclosure2 :: AFNEp -> [String] -> [String]
eclosure2 _ [] = [] --Caso base
--Toma el primer estado, calcula su e-closure, lo mismo para el reso, concatena; al final limpia duplicados.
eclosure2 m (x:xs) = rmDup (eclosure (transiciones m) m x ++ eclosure2 m xs) 

--Toma un solo estado y devuelve todos los estados a los que puede ir leyendo el símbolo
do_trans_nep :: [Trans_eps] -> Char -> String -> [String]
do_trans_nep [] _ _ = []
do_trans_nep ((q1, Just c1, q2):xs) c2 q3 --Si la transicion es con un simbolo,comparamos:
  | q1 == q3 && c1 == c2 = rmDup (q2 ++ do_trans_nep xs c2 q3) --si coincide agregamos el destino
  | otherwise = do_trans_nep xs c2 q3 --si no coincide seguimos buscando
do_trans_nep (_:xs) c2 q3 = do_trans_nep xs c2 q3  -- si la transicion es epsilon, la ignoramos

--Toma un conjunto de estados y devuelve todos los estados a los que puede ir leyendo el símbolo
do_trans_nep2 :: [Trans_eps] -> Char -> [String] -> [String]
do_trans_nep2 l c qs = rmDup (concatMap (do_trans_nep l c) qs)

--crea todas las transiciones del AFN resultante
trans_eps_to_afn :: AFNEp -> [Trans_afn]
trans_eps_to_afn m =
  concatMap (trans_eps_to_afn_aux m (transiciones m) (alfabeto m)) (estados m)

--Recibe el autómata, sus transiciones, el alfabeto y un estado q; regresa todas las transiciones desde q para cada símbolo.
trans_eps_to_afn_aux :: AFNEp -> [Trans_eps] -> String -> String -> [Trans_afn]
trans_eps_to_afn_aux _ _ [] _ = []
trans_eps_to_afn_aux m l (c:cs) q =
  (q, c, qn) : trans_eps_to_afn_aux m l cs q
  where
    -- 1) ε-closure(q)
    cq   = eclosure l m q
    -- 2) δ(cq, c)
    paso = do_trans_nep2 l c cq
    -- 3) ε-closure(δ(...))
    qn   = eclosure2 m paso

--calcula los finales del AFN a partir de los finales del AFNε
finalesDesdeCerradura :: AFNEp -> [String]
finalesDesdeCerradura m = [ q | q <- estados m, final m `elem` eclosure (transiciones m) m q ]

afnEp_to_AFN :: AFNEp -> AFN
afnEp_to_AFN m = AFN{ estadosN = estados m, alfabetoN = alfabeto m,
                    transicionesN = trans_eps_to_afn m,
                    inicialN = inicial m, finalesN = finalesDesdeCerradura m
}