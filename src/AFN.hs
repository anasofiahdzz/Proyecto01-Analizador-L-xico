-- >>>>> Definición de AFN y la función para pasar de AFNepsilon --> AFN <<<<<
module AFN (AFN(..), Trans_afn, aFNEp_to_AFN) where


-- Se importa la definición de AFNEp.
import AFNEp
-- Usamos Set para hacer conjuntos de estados.
import qualified Data.Set as Conj


-- Definición de una transición con un símbolo.
-- Estado origen -> Símbolo -> Lista de estados destino
type Trans_afn = (Estado, Char, [Estado])


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
      ["\n----------------------------------------------------------------------" 
      ,"                 ***** Imprimiendo AFN *****" 
      , "\n>> estados      = " ++ show (estados2 m)
      , "\n>> alfabeto     = " ++ show (alfabeto2 m)
      , "\n>> transiciones = " ++ show (transiciones2 m)
      , "\n>> inicial      = " ++ show (inicial2 m)
      , "\n>> finales      = " ++ show (finales2 m)
      , "\nListo :D"
      ,"----------------------------------------------------------------------"
      ]
      



-- ### (1) Calculo de las E-closure ###



-- \\\ Cálculo de la E-closure para un estado q ///
eclosure :: AFNEp -> Estado -> Conj.Set Estado
eclosure m q =
    -- La idea es implementar una búsqueda tipo DFS para encontrar los
    -- estados alcanzables. Dicha implementación recibe la lista de estados por
    -- revisar y el conjunto de estados que ya hemos procesado.
    
    -- Iniciamos dfs con el estado q tanto en los estados por revisar,
    -- como en los estados visitados. Singleton para un solo un elemento.
    dfs [q] (Conj.singleton q)
    
  where
    -- EstadosPorRevisar -> EstadosVisitados -> EstadosAlcanzables
    dfs :: [Estado] -> Conj.Set Estado -> Conj.Set Estado
    -- Caso base. No hay más estados por procesar.
    -- Simplemente devolvemos el conjunto de estados visitados.
    dfs [] visitados = visitados
    -- Caso recursivo.
    dfs (qi : estados) visitados =
          -- Vemos todos los vecinos alcanzables desde qi con epsilon.
          let vecinosAlcanzables = Conj.fromList [ qDestino | (qOrigen, Nothing, qDestinos) <- transiciones m, qOrigen == qi, qDestino <- qDestinos ]
              -- Filtramos los veciParanos que no hayamos visitado. Difference para la resta entre conjuntos.
              nuevosVecinos = Conj.difference vecinosAlcanzables visitados
              -- Agregamos estos nuevos vecinos al resto de estados por procesar.
              nuevosPorRevisar = Conj.toList nuevosVecinos ++ estados
              -- Agregamos estos nuevos vecinos al conjunto de 'visitados'.
              nuevosVisitados = Conj.union visitados nuevosVecinos
          -- Continuamos la búsqueda recursiva.
          in dfs nuevosPorRevisar nuevosVisitados



-- \\\ Función que calcula la E-closure para un conjunto de estados ///
-- Digamos que simplemente es la unión de las E-closure de cada estado en la lista.
eclosure_Union :: AFNEp -> Conj.Set Estado -> Conj.Set Estado
-- con foldMap aplicamos (eclosure m) a cada estado 'q' en 'estados', 
-- para al final unir todos los conjuntos resultantes.
eclosure_Union m estados = foldMap (eclosure m) estados



-- ### (2) Cálculo de Delta ###

                            

-- \\\ Función que calcula la función delta para un conjunto de estados y un símbolo ///
delta :: AFNEp -> Conj.Set Estado -> Char -> Conj.Set Estado
delta m setEstados a =
  -- con S.fromList nos encargamos de los duplicados.
  Conj.fromList [ dest | (q, Just c, listaDestinos) <- transiciones m, -- Para las transiciones.
                        q `Conj.member` setEstados, -- donde q esté en nuestro conjunto de origen
                        c == a, -- y el símbolo sea 'a'
                        dest <- listaDestinos ] -- y el símbolo sea 'a'
                            
                            
                            
-- ### (3) Cálculo de la nueva función de transición delta' ###
--  d'(q, a) = ECLOSURE( d( ECLOSURE(q), a ) ) 



-- \\\ Función para generar a lista de todas las transiciones para el nuevo AFN ///
transicionesAFN :: AFNEp -> [Trans_afn]
transicionesAFN m =
  -- Usamos una lista para iterar sobre cada estado 'q' y cada símbolo 'a'.
  [ (q, a, Conj.toList deltaFinal) -- Creamos la transición (q, a, destinos).
  | q <- estados m, -- Para cada estado 'q' en el autómata.
    a <- alfabeto m, -- Para cada símbolo 'a' en el alfabeto.
    let ecQ = eclosure m q  -- Calculamos EC(q).
        delta_ecQ = delta m ecQ a    -- Calculamos d(EC(q), a).
        deltaFinal = eclosure_Union m delta_ecQ -- Calculamos EC( d(EC(q), a)).
  , not (Conj.null deltaFinal) ]



-- ### (4) Nuevos estados finales ###



-- \\\ Función para calcular los estados finales en el nuevo AFN ///
nuevosFinales :: AFNEp -> [Estado]
nuevosFinales m = 
    -- Primero obtenemos el conjunto de estados finales del AFNEp que tenemos como entrada.
    let antiguosFinales = Conj.fromList (finales m)
    -- Iteramos sobre los estado 'q' del autómata.
    in [ q | q <- estados m,
           -- Calculamos la E-closure de 'q'.
           let ec_q = eclosure m q,
           -- Vemos si la intersección entre ec_q y antiguosFinales no está vacía.
           -- S.disjoint(A, B) nos dice si la intersección es vacia. Si lo negamos,
           -- entonces nos dice si la intersección no esta vacía.
           not (Conj.disjoint ec_q antiguosFinales) ]
           
           
          
-- ### (5) Ensamble final para crear un AFN apartir de AFNEp



-- <<< Función principal. Pasa de AFNEp a AFN >>>
aFNEp_to_AFN :: AFNEp -> AFN
aFNEp_to_AFN m = AFN {
    -- Los estados, el alfabeto y el estado inicial no cambian.
    estados2 = estados m,
    alfabeto2 = alfabeto m,
    inicial2 = inicial m,
    -- calculamos las nuevas transiciones y los nuevos estados finales.
    transiciones2 = transicionesAFN m,
    finales2 = nuevosFinales m
  }
