module AFNEp where
import ER --para usar tipo Expr

--type Estado = String
--type Trans_eps = (String, Maybe Char, [String]) -- (estado origen, símbolo de : epsilon, [estado destino]) ya definido en ER.hs

-- Automáta no determinista con transiciones epsilon, 
data AFNEp = AFNEp {estados :: [String], alfabeto :: [Char], --lista de estados, string, los alfabetos son chars
                  transiciones :: [Trans_eps], --lista de transiciones epsilon
                  inicial :: String, final :: String} deriving (Show) -- estdo inicial y final y clase show para imprimir el automata

-- | Traducción de expresiones regulares a automátas con transiciones epsilón, usando recursion

-- los estados a los que se llegan: q0, q1, q2, ...
estadoNuevo :: Int -> String --fun que genera los nombres de los estados nuevos
estadoNuevo edoNuevo = "q" ++ show edoNuevo

-- expr_to_AFNEp :: Expr -> AFNEp
expr_to_AFNEp :: Expr -> Int -> (AFNEp, Int) -- usa un contador de estados
-- Crea un AFNe con dos estados y una transicion por el caracter c. Simbolo simple
expr_to_AFNEp (Term c) edoNuevo = -- automata : q0 --c--> q1
    let q0 = estadoNuevo edoNuevo --edo inicial
        q1 = estadoNuevo (edoNuevo + 1) --edo final
    in (AFNEp [q0,q1] [c] [(q0, Just c, [q1])] q0 q1, edoNuevo+2) --automata y actualiza el contador

-- Crea un AFNe con dos estados y una transicion por cada caracter en el rango [a..b]
expr_to_AFNEp (Range a b) edoNuevo = -- lo mismo que para term pero hace transicion en cada estado
    let chars = [a..b]
        q0 = estadoNuevo edoNuevo
        q1 = estadoNuevo (edoNuevo+1)
        transicion = [(q0, Just c, [q1]) | c <- chars] --aqui las tranciciones para cada caracter
    in (AFNEp [q0,q1] chars transicion q0 q1, edoNuevo+2) --automata y actualiza el contador

-- Crea un nuevo estado inicial y final, conecta ambos subautomatas con transiciones epsilon desde el nuevo inicial y hacia el nuevo final.
expr_to_AFNEp (Or expr1 expr2) edoNuevo =
    let (a1, edoNuevo1) = expr_to_AFNEp expr1 edoNuevo --convierte la primer expresion reg enun automata
        (a2, edoNuevo2) = expr_to_AFNEp expr2 edoNuevo1  --convierte la segunda expresion reg en un automata
        q0 = estadoNuevo edoNuevo2 --nuevo edo inicial
        qf = estadoNuevo (edoNuevo2 + 1) --nuevo edo final
        epsilons = --lista de las trans eps para conectar los subautomatas
            [ (q0, Nothing, [inicial a1])
            , (q0, Nothing, [inicial a2])
            , (final a1, Nothing, [qf])
            , (final a2, Nothing, [qf])
            ]
    in (AFNEp (q0:qf:estados a1 ++ estados a2)
              (alfabeto a1 ++ alfabeto a2)
              (transiciones a1 ++ transiciones a2 ++ epsilons)
              q0 qf, edoNuevo2 + 2) --automata y actualiza el contador

-- Conecta el final del primer subautomata con el inicial del segundo usando una transicion epsilon.
expr_to_AFNEp (And expr1 expr2) edoNuevo =
    let (a1, edoNuevo1) = expr_to_AFNEp expr1 edoNuevo --convierte la primer expresion reg enun automata
        (a2, edoNuevo2) = expr_to_AFNEp expr2 edoNuevo1 --convierte la segunda expresion reg en un automata
        epsilons = [(final a1, Nothing, [inicial a2])] --la e que conecta el final de a1 con el inicio de a2
    in (AFNEp (estados a1 ++ estados a2)
              (alfabeto a1 ++ alfabeto a2)
              (transiciones a1 ++ transiciones a2 ++ epsilons)
              (inicial a1) (final a2), edoNuevo2) --automata y actualiza el contador

-- Añade nuevos estados inicial y final, conecta el subautomata para permitir repeticiones (ciclos) y la posibilidad de aceptar la cadena vacia, con transiciones epsilon.
expr_to_AFNEp (Kleene expr) edoNuevo =
    let (a1, edoNuevo1) = expr_to_AFNEp expr edoNuevo --convierte la primer expresion reg enun automata
        q0 = estadoNuevo edoNuevo1 --nuevo edo inicial
        qf = estadoNuevo (edoNuevo1 + 1) --nuevo edo final
        epsilons = --lista de trans eps que acepta repeticiones y cadena vacia
            [ (q0, Nothing, [qf])
            , (q0, Nothing, [inicial a1])
            , (final a1, Nothing, [inicial a1])
            , (final a1, Nothing, [qf])
            ]
    in (AFNEp (q0:qf:estados a1)
              (alfabeto a1)
              (transiciones a1 ++ epsilons)
              q0 qf, edoNuevo1 + 2) --automata y actualiza el contador


