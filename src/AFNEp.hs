-- >>>>> Definición de AFNepsilon y la función para pasar de ER --> AFNepsilon <<<<<
module AFNEp where


-- Se importa la definición de las ER.
import ER 
-- nub para eliminar duplicados.
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
    finales :: [Estado] -- Lista de estados finales.
    } deriving (Eq) -- Para comparar dos de estos autómatas.

-- Show personalizado, para poder imprimir mejor nuestra autómata.
instance Show AFNEp where
  show m =
    unlines
      ["\n----------------------------------------------------------------------" 
      ,"                 ***** Imprimiendo AFNEp *****" 
      , "\n>> estados      = " ++ show (estados m)
      , "\n>> alfabeto     = " ++ show (alfabeto m)
      , "\n>> transiciones = " ++ show (transiciones m)
      , "\n>> inicial      = " ++ show (inicial m)
      , "\n>> finales      = " ++ show (finales m)
      , "\nListo :D"
      ,"----------------------------------------------------------------------"
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
    in (AFNEp [q0, q1] [c] [transicion] q0 [q1], nuevesito + 2)

-- Caso 2 (Base) : Más de un símbolo. Regresamos un autómata de tipo q0 -> c -> q1 para cada símbolo.
expr_to_AFNEp (Range a b) nuevesito =
    let simbolos = [a..b] -- Lista de símbolos.
        q0 = estadoNuevo nuevesito -- Estado inicial q0.
        q1 = estadoNuevo (nuevesito + 1) -- Estado final q1.
        transicion = [(q0, Just c, [q1]) | c <- simbolos] -- Lista de transiciones para todos los símbolos.
    -- Se devuelve el autómata y el contador actualizado en 2.
    in (AFNEp [q0, q1] simbolos transicion q0 [q1], nuevesito + 2)

-- Caso 3 : expr1 + expr2. 
-- Construimos los autómatas m1 para 'expr1' y m2 para 'expr2' por separado, para luego conectarlos con transiciones epsilon.
expr_to_AFNEp (Or expr1 expr2) nuevesito =
    let (m1, estado1) = expr_to_AFNEp expr1 nuevesito -- Construimos el autómata m1 para expr1.
        (m2, estado2) = expr_to_AFNEp expr2 estado1 -- Construimos el autómata m2 para expr2.
        -- Nuevo estado inicial q0.
        q0 = estadoNuevo estado2
        -- Definimos las 2 nuevas transiciones epsilon para conectarlos.
        transEp = [ (q0, Nothing, [inicial m1]), (q0, Nothing, [inicial m2])]
        -- Definimos los componentes del nuevo autómata.
        nuevosEstados = q0 : (estados m1 ++ estados m2)
        nuevoAlfabeto = unionAlfabetos (alfabeto m1) (alfabeto m2)
        nuevasTransiciones = transiciones m1 ++ transiciones m2 ++ transEp
        nuevosFinales = finales m1 ++ finales m2
    -- Devuelve el autómata 'Or' completo y el contador actualizado.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones q0 nuevosFinales, estado2 + 1) 

-- Caso 4 : expr1expr2.
-- Construye m1 y m2, y luego conectamos el final de m1 con el inicio de m2 usando una transición epsilon.
expr_to_AFNEp (And expr1 expr2) nuevesito =
    let (m1, estado1) = expr_to_AFNEp expr1 nuevesito -- Construimos el autómata m1 para expr1.
        (m2, estado2) = expr_to_AFNEp expr2 estado1 -- Construimos el autómata m2 para expr2.
        -- Nuevas transiciones epsilon desde cada estado final de m1 hacia el inicial de m2.
        transEp = [(qf_m1, Nothing, [inicial m2]) | qf_m1 <- finales m1]
        -- Sin estados nuevos.
        nuevosEstados = estados m1 ++ estados m2
        nuevoAlfabeto = unionAlfabetos (alfabeto m1) (alfabeto m2)
        nuevasTransiciones = transiciones m1 ++ transEp ++ transiciones m2
    -- Devuelve el autómata 'And' completo. No se actualiza el contador porque no usamos estados nuevos.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones (inicial m1) (finales m2), estado2) 

-- Caso 5 : expr^*.
-- Construye el autómata para expr y lo rodeamos con transiciones epsilon para permitir 0 o más repeticiones.
expr_to_AFNEp (Kleene expr) nuevesito =
    let (m, estado) = expr_to_AFNEp expr nuevesito -- Construimos el autómata m para expr.
        -- Transiciones epsilon desde cada final de vuelta al inicial y de q0 al inicial de m.
        transEp = [(qf_m, Nothing, [inicial m]) | qf_m <- finales m]
        -- Los estados y alfabeto siguen siendo los mismos.
        nuevosEstados = estados m
        nuevoAlfabeto = alfabeto m
        -- Juntamos las transiciones.
        nuevasTransiciones = transiciones m ++ transEp
        -- El estado inicial de m ahora es un estado final.
        -- nub para evitar duplicados si el estado inicial m ya era final.
        nuevosFinales = nub (inicial m : finales m)
    -- Devuelve el autómata 'Kleene' completo y el contador actualizado.
    in (AFNEp nuevosEstados nuevoAlfabeto nuevasTransiciones (inicial m) nuevosFinales, estado)
