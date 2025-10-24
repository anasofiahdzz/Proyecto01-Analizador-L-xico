-- >>>>> Definición de AFNepsilon y la función para pasar de ER --> AFNepsilon <<<<<
module AFNEp where


-- Se importa la definición de las ER.
import ER 
-- nub para eliminar duplicados del alfabeto.
import Data.List (nub)


-- Trataremos a los Estados como String para que sea más legible.
type Estado = String
-- Definición de una transición para una AFNepsilon. 
-- Tenemos un Estado origen, un símbolo o nada (epsilon), y una lista de Estados destino.
type Trans_eps = (Estado, Maybe Char, [Estado])


-- Definición de un Autómata Finito No Determinista con transiciones epsilon.
data AFNEp = AFNEp {
    estados :: [Estado], -- Lista de todos los estados.
    alfabeto :: [Char], -- Alfabeto que acepta el autómata.
    transiciones :: [Trans_eps], -- Reglas de transición.
    inicial :: Estado, -- Estado inicial.
    final :: Estado -- Un estado final. Lo dejamos asi?...
    } deriving (Eq) -- Para comparar dos de estos autómatas.

-- Show personalizado, para poder imprimir mejor nuestra autómata.
instance Show AFNEp where
  show m =
    unlines
      [ "AFNEp {"
      , " >> estados      = " ++ show (estados m)
      , " >> alfabeto     = " ++ show (alfabeto m)
      , " >> transiciones = " ++ show (transiciones m)
      , " >> inicial      = " ++ show (inicial m)
      , " >> final        = " ++ show (final m)
      , "}"
      ]


-- ### Función auxiliar para generar nombres de estados ###
estadoNuevo :: Int -> Estado
-- Tomamos un entero N y regresa el nuevo estado en el formato "qN"
estadoNuevo nuevo = "q" ++ show nuevo



-- ### Función auxiliar para unir los alfabetos de dos autómatas ###
unionAlfabetos :: [Char] -> [Char] -> [Char]
-- nub para eliminar símbolos duplicados en el nuevo alfabeto.
unionAlfabetos s1 s2 = nub (s1 ++ s2)



-- ### Función de azúcar sintactica, que convierte una ER a AFNepsilon ### Esto lo dejaremos?...
compilarAFNEp :: Expr -> AFNEp
-- Los estados tienen que comenzar en 0.
compilarAFNEp e = fst (expr_to_AFNEp e 0)



-- ========== Implementación de Algoritmo de Thompson ========== 



-- <<< Función recursiva que hace toda la chamba. Se usa un contador de estados >>>
expr_to_AFNEp :: Expr -> Int -> (AFNEp, Int) 

-- Caso 1 (Base) : Un solo símbolo. Regresaremos un autómata de tipo q0 -> c -> q1.
expr_to_AFNEp (Term c) nuevesito =
    let q0 = estadoNuevo nuevesito -- Estado inicial q0.
        q1 = estadoNuevo (nuevesito + 1) -- Estado final q1.
        transicion = (q0, Just c, [q1]) -- La única transición es con el símbolo c. 
    -- Se devuelve el autómata.
    -- La lista de estados "[q0, q1]", el alfabeto "[c]", la única transición, 
    -- el estado inicial q0, el estado final q1. 
    in (AFNEp [q0, q1] [c] [transicion] q0 q1, edoNuevo + 2)

-- Caso 2 (Base) : Más de un símbolo. Regresamos un autómata de tipo q0 -> c -> q1 para cada símbolo.
expr_to_AFNEp (Range a b) edoNuevo =
    let simbolos = [a..b] -- Lista de símbolos.
        q0 = estadoNuevo edoNuevo -- Estado inicial q0.
        q1 = estadoNuevo (edoNuevo+1) -- Estado final q1.
        transicion = [(q0, Just c, [q1]) | c <- simbolos] -- Lista de transiciones para todos los símbolos.
    -- Se devuelve el autómata y el contador actualizado en 2.
    in (AFNEp [q0, q1] simbolos transicion q0 q1, edoNuevo + 2)

-- Caso 3 : expr1 + expr2. 
-- Construimos los autómatas m1 para 'expr1' y m2 para 'expr2' por separado, para luego conectarlos con transiciones epsilon.
expr_to_AFNEp (Or expr1 expr2) edoNuevo =
    let (m1, edoNuevo1) = expr_to_AFNEp expr1 edoNuevo -- Construimos el autómata m1 para expr1.
        (m2, edoNuevo2) = expr_to_AFNEp expr2 edoNuevo1 -- Construimos el autómata m2 para expr2.
        -- Los nuevos estados de inicio y fin para el Or.
        q0 = estadoNuevo edoNuevo2
        qf = estadoNuevo (edoNuevo2 + 1)
        -- Definimos las 4 nuevas transiciones epsilon para conectarlos.
        transEp = 
            [ (q0, Nothing, [inicial m1])
            , (q0, Nothing, [inicial m2])
            , (final m1, Nothing, [qf])
            , (final m2, Nothing, [qf])
            ]
        -- Definimos los componentes del nuevo autómata.
        nuevosEstados = q0 : qf : estados m1 ++ estados m2
        nuevoAlfabeto = unionAlfabetos (alfabeto m1) (alfabeto m2)
        nuevasTransiciones = transiciones m1 ++ transiciones m2 ++ transEp
    -- Devuelve el autómata 'Or' completo y el contador actualizado.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones q0 qf, edoNuevo2 + 2) 

-- Caso 4 : expr1expr2.
-- Construye m1 y m2, y luego conectamos el final de m1 con el inicio de m2 usando una transición epsilon.
expr_to_AFNEp (And expr1 expr2) edoNuevo =
    let (m1, edoNuevo1) = expr_to_AFNEp expr1 edoNuevo -- Construimos el autómata m1 para expr1.
        (m2, edoNuevo2) = expr_to_AFNEp expr2 edoNuevo1 -- Construimos el autómata m2 para expr2.
        transEp = [(final m1, Nothing, [inicial m2])] -- La transición epsilon que conecta el final de m1 con el inicio de m2.
        -- Sin estados nuevos.
        nuevosEstados = estados m1 ++ estados m2
        nuevoAlfabeto = unionAlfabetos (alfabeto m1) (alfabeto m2)
        nuevasTransiciones = transiciones m1 ++ transEp ++ transiciones m2
    -- Devuelve el autómata 'And' completo. No se actualiza el contador porque no usamos estados nuevos.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones (inicial m1) (final m2), edoNuevo2) 

-- Caso 5 : expr^*.
-- Construye el autómata para expr y lo rodeamos con transiciones epsilon para permitir 0 o más repeticiones.
expr_to_AFNEp (Kleene expr) edoNuevo =
    let (m, edoNuevo1) = expr_to_AFNEp expr edoNuevo -- Construimos el autómata m para expr.
        -- Nuevos estados.
        q0 = estadoNuevo edoNuevo1 -- inicial.
        qf = estadoNuevo (edoNuevo1 + 1) -- final.
        transEp = -- lista de trans eps que acepta repeticiones y cadena vacia.
            [ (q0, Nothing, [qf])
            , (q0, Nothing, [inicial m])
            , (final m, Nothing, [inicial m])
            , (final m, Nothing, [qf])
            ]
        nuevosEstados = q0 : qf : estados m
        nuevoAlfabeto = alfabeto m
        nuevasTransiciones = transiciones m ++ transEp
    -- Devuelve el autómata 'Kleene' completo y el contador actualizado.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones q0 qf, edoNuevo1 + 2)
