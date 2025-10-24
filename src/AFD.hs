-- >>>>> Definición de AFD y la función para pasar de AFN --> AFD <<<<<
module AFD where

-- Importamos la definición de AFN que creamos
import AFN

-- ¡CAMBIO! Importa el módulo 'AFNEp' completo
-- (esto nos da el tipo 'Estado' Y la función 'compilarAFNEp')
import AFNEp

-- ¡CAMBIO! Importamos 'nub' e 'intercalate'
import Data.List (nub, intercalate)

-- ¡CAMBIO! Importamos 'ER' para poder definir la ER de prueba
import ER

-- Transición determinista. 
-- Estado origen -> Símbolo -> Estado destino.
type Trans_afd = (Estado, Char, Estado) 

-- Definición de Autómata determinista.
data AFD = AFD { 
    estadosD :: [Estado], -- Lista de todos los estados.
    alfabetoD :: [Char], -- Alfabeto que acepta el autómata.
    transicionesD :: [Trans_afd], -- Reglas de transición.
    inicialD :: Estado, -- Estado inicial.
    finalesD :: [Estado] -- Estados finales
    } deriving (Eq)

-- Show personalizado, para poder imprimir mejor nuestra autómata.
instance Show AFD where
  show m =
    unlines
      [ "AFD {"
      , "  estados      = " ++ show (estadosD m)
      , "  alfabeto     = " ++ show (alfabetoD m)
      , "  transiciones = " ++ show (transicionesD m)
      , "  inicial      = " ++ show (inicialD m)
      , "  finales      = " ++ show (finalesD m)
      , "}"
      ]
      
      
-- ### Función auxiliar que convierte una lista de estados en un nombre ###
renombra :: [Estado] -> Estado
renombra = intercalate ","

-- ========== Lógica para pasar de AFN a AFD ========== 

-- ### Función auxiliar que calcula la delta para un conjunto de Estados y un símbolo a ###
alcanzables :: AFN -> [Estado] -> Char -> [Estado]
alcanzables afn conj a =
  -- Recorremos todas las transiciones de AFN. Luego las filtramos las que estan en nuestra lista de
  -- estados y usan el símbolo a. Recolectamos todos los estados destino y finalmente eliminamos duplicados.
  nub [ destino | (q, c', ds) <- transiciones2 afn, q `elem` conj , c' == a , destino <- ds ]
        

-- ### Función auxiliar que descubre todos los estados alcanzables del nuevo AFD.
-- Regresamos los pares ([estados en AFN], "nombre Estado AFD")
descubrirEstados :: AFN -> [([Estado], Estado)]
descubrirEstados afn =
    -- Definimos el estado inicial del AFD, que es el conjunto que solo contiene al inicial del AFN.
    let inicialSet = [inicial2 afn]
        inicialStr = renombra inicialSet
    -- Encontramos recursivamente el resto con procesa.
    in procesa afn [(inicialSet, inicialStr)] []    
    
-- ### Función auxiliar que hace la búsqueda de estados alcanzables ###
-- Necesitamos el AFN, la lista de estados pendientes por procesar y la lista de estados visitados.
procesa :: AFN -> [([Estado], Estado)] -> [([Estado], Estado)] -> [([Estado], Estado)]
-- Nuestro caso base será que no hay más pendientes, así que regresamos los visitados.
procesa _   [] visitados = visitados
procesa afn ((conj, nombre):pendientes) visitados
  -- Si el estado actual ya fue visitado, entonces lo saltamos y seguimos con los pendientes.
  | nombre `elem` map snd visitados = procesa afn pendientes visitados
  -- eoc.
  | otherwise =
      -- Calculamos todos los nuevos estados a los que se puede llegar desde conj.
      let nuevos = [ (alcanzables afn conj c, renombra (alcanzables afn conj c))
                   | c <- alfabeto2 afn , not (null (alcanzables afn conj c))]
      -- Llamada recursiva, añadiendo nuevos a la lista de pendientes y moviendo el estado actual
      -- a la lista de los visitados.
      in procesa afn (pendientes ++ nuevos) (visitados ++ [(conj, nombre)])
        
-- ### Función auxiliar para genera la lista de todas las transiciones del nuevo AFD ###
calcularTransiciones :: AFN -> [([Estado], Estado)] -> [Trans_afd]
calcularTransiciones afn todosEstados =
  [ (nombre, c, renombra (alcanzables afn conjunto c))
  | (conjunto, nombre) <- todosEstados , c <- alfabeto2 afn
  , not (null (alcanzables afn conjunto c))]
        
-- ### Función auxiliar que fenera la lista de los estados finales del nuevo AFD ###
calcularFinales :: AFN -> [([Estado], Estado)] -> [Estado]
calcularFinales afn todosEstados =
  [ nombre | (conjunto, nombre) <- todosEstados , any (`elem` finales2 afn) conjunto ]
        
        
-- <<< Función que convierte un AFN a un AFD >>>
aFN_to_AFD :: AFN -> AFD
aFN_to_AFD afn =
    -- Descubrimos todos los estados alcanzables.
    let todosEstados = descubrirEstados afn
        -- Calculamos el estado inicial
        estadoInicial = snd (head todosEstados)
        -- Calculamos todas las transiciones del AFD
        transiciones = calcularTransiciones afn todosEstados
        -- Calculamos los estados finales del AFD
        estadosFinales = calcularFinales afn todosEstados
        -- Obtenemos la lista final de nombres de estados
        listaEstados = map snd todosEstados
        
    -- Al final, ensamblamos todo
    in AFD { estadosD = listaEstados,
             alfabetoD = alfabeto2 afn,
             transicionesD = transiciones,
             inicialD = estadoInicial,
             finalesD = estadosFinales }
