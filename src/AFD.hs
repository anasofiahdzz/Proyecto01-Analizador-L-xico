-- >>>>> Definición de AFD y la función para pasar de AFN --> AFD <<<<<
module AFD where


-- Importamos la definición de AFN y el dato Estado
import AFN
import AFNEp (Estado)

import Data.List (nub, intercalate, sort)



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
      ["\n----------------------------------------------------------------------" 
      ,"                 ***** Imprimiendo AFD *****" 
      , "\n>> estados      = " ++ show (estadosD m)
      , "\n>> alfabeto     = " ++ show (alfabetoD m)
      , "\n>> transiciones = " ++ show (transicionesD m)
      , "\n>> inicial      = " ++ show (inicialD m)
      , "\n>> finales      = " ++ show (finalesD m)
      , "\nListo :D"
      ,"----------------------------------------------------------------------"
      ]
      
      
-- ### Función auxiliar que convierte una lista de estados en un nombre ###
renombra :: [Estado] -> Estado
-- Ordenamos la lista antes de unirla
renombra = intercalate "," . sort



-- ========== Lógica para pasar de AFN a AFD ========== 



-- \\\ Función que calcula la delta para un conjunto de Estados y un símbolo a ///
alcanzables :: AFN -> [Estado] -> Char -> [Estado]
alcanzables afn conj a =
  -- Recorremos todas las transiciones de AFN. Luego filtramos las que estan en nuestra lista de
  -- estados y usan el símbolo a. Recolectamos todos los estados destino y finalmente eliminamos duplicados.
  nub [ destino | (q, c', ds) <- transiciones2 afn, q `elem` conj , c' == a , destino <- ds ]
        


-- \\\ Función que descubre todos los estados alcanzables del nuevo AFD ///
-- Vamos a regresar los pares ([estados en AFN], "nombre Estado AFD")
descubrirEstados :: AFN -> [([Estado], Estado)]
descubrirEstados afn =
    -- Definimos el estado inicial del AFD, que es el conjunto que solo contiene al inicial del AFN.
    let conjuntoInicial = [inicial2 afn]
        -- Nuevo nombre
        nombre = renombra conjuntoInicial
    -- Encontramos recursivamente el resto con la función procesa.
    in procesa afn [(conjuntoInicial, nombre)] []    
    
    

-- \\\ Función que hace la búsqueda de estados alcanzables ///
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
        
        
        
-- \\\ Función para genera la lista de todas las transiciones del nuevo AFD ///
calcularTransiciones :: AFN -> [([Estado], Estado)] -> [Trans_afd]
calcularTransiciones afn todosEstados =
  [ (nombre, c, renombra (alcanzables afn conjunto c)) -- Para cada nuevo estado (conjunto, nombre),
  | (conjunto, nombre) <- todosEstados , c <- alfabeto2 afn -- para cada carácter c del alfabeto, 
  , not (null (alcanzables afn conjunto c))] -- y si la transición nos lleva a algún lado, pues creamos la transición determ.
        
        
        
-- \\\ Función auxiliar que fenera la lista de los estados finales del nuevo AFD ///
calcularFinales :: AFN -> [([Estado], Estado)] -> [Estado]
calcularFinales afn todosEstados =
  -- Un estado del AFD es final si su conjunto de estados tiene alguna intersección 
  -- con los estados finales del AFN.
  [ estadoF | (conjunto, estadoF) <- todosEstados , any (`elem` finales2 afn) conjunto ]
  


-- \\\ Función auxiliar para ver si existe una transición ///
existeTrans :: [Trans_afd] -> Estado -> Char -> Bool
-- Nos detenemos en cuanto encontremos una transición que cumpla la condición.
existeTrans trans q c = any (\(q', c', _) -> q' == q && c' == c) trans



-- \\\ Función que completa el AFD añadiendo un estado sumidero ///
completarAFD :: AFD -> AFD
completarAFD afd =
    let estados = estadosD afd
        alfabeto = alfabetoD afd
        transiciones = transicionesD afd
        -- Nuevo estado sumidero
        estadoSumidero = "Sumidero" 
        
        -- Encontramos todas las transiciones faltantes.
        transicionesFaltantes = [ (q, c, estadoSumidero) 
                                | q <- estados, c <- alfabeto
                                , not (existeTrans transiciones q c) ]
        
        -- Si no faltan transiciones, devolvemos el AFD original.
        (estadosNuevos, transicionesNuevas)
            | null transicionesFaltantes = (estados, transiciones)
            | otherwise = 
                -- Si faltan, añadimos el estado sumidero y sus transiciones.
                -- Todas las transicicones que salen del sumidero, dan al mismo sumidero.
                let transError = [ (estadoSumidero, c, estadoSumidero) | c <- alfabeto ]
                in (estados ++ [estadoSumidero], transiciones ++ transicionesFaltantes ++ transError)

    -- Devolvemos el AFD actualizado
    in afd { estadosD = estadosNuevos, transicionesD = transicionesNuevas }



-- \\\ Función que renombra todos los estados a q_i ///
renombrarAFD :: AFD -> AFD
renombrarAFD afd =
    let inicialAntiguo = inicialD afd
        -- Generamos una lista ordenada de los estados antiguos.
        estadosAntiguos = sort (nub (estadosD afd))
        
        -- Lo que queremos hacer es una lista de los siguientes pares -> (Antiguo nombre, Nuevo nombre)
        estadosSinInicial = filter (/= inicialAntiguo) estadosAntiguos
        -- Nos aseguramos de que el estado inicial siempre sea "q0"
        inicial = (inicialAntiguo, "q0")
        -- Empezamos el renombramiento.
        resto = zip estadosSinInicial ["q" ++ show i | i <- [1..]]
        nuevosNombres = inicial : resto

        -- Ya hay que hacerlo aquí mismo...
        -- Función auxiliar para buscar el nuevo nombre.
        buscarNombre :: Estado -> Estado
        buscarNombre qAntiguo = 
            case lookup qAntiguo nuevosNombres of
                Just qNuevo -> qNuevo
                -- Just in case...
                Nothing -> qAntiguo 
                
        -- Aplicamos los nuevos nombres a todos los componentes del AFD
        nuevosEstados = sort (map snd nuevosNombres) 
        nuevoInicial = "q0"
        nuevosFinales = sort (nub (map buscarNombre (finalesD afd)))
        nuevasTransiciones = [ (buscarNombre q, c, buscarNombre qd)
                             | (q, c, qd) <- transicionesD afd ]
    
    in AFD { estadosD = nuevosEstados,
             alfabetoD = alfabetoD afd,
             transicionesD = nuevasTransiciones,
             inicialD = nuevoInicial,
             finalesD = nuevosFinales }
        
        
-- <<< Función principal. Convierte un AFN a un AFD >>>
aFN_to_AFD :: AFN -> AFD
aFN_to_AFD afn =
    -- Construcción por subconjuntos.
    let todosEstados = descubrirEstados afn
        estadoInicial = snd (head todosEstados)
        transiciones = calcularTransiciones afn todosEstados
        estadosFinales = calcularFinales afn todosEstados
        listaEstados = map snd todosEstados
        
        afdIntermedio = AFD { estadosD = listaEstados,
                             alfabetoD = alfabeto2 afn,
                             transicionesD = transiciones,
                             inicialD = estadoInicial,
                             finalesD = estadosFinales }

    --  Completar el AFD.
        afdCompleto = completarAFD afdIntermedio

    -- Renombrar estados.
        afdFinal = renombrarAFD afdCompleto
        
    in afdFinal
