-- >>>>> Definición de AFDmin y la función para pasar de AFD --> AFDmin <<<<<
module AFDmin where

-- Importamos la definición de AFD.
import AFD
-- Importamos el Estado.
import AFNEp (Estado)

-- Usaremos Map y Set para una implementación eficiente de la tabla y de los conjuntos de estados.
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- find para buscar y sort para renombrar.
import Data.List (sort, find, nub)

-- **** Tipos auxiliares ****

-- Par de estados (p, q) donde p < q. Esto para usarlo en un Mapa.
type Par = (Estado, Estado)

-- Implementación de la tabla que debemos ir marcando como lo vimos en clase. La vamos a ir mapeando con boolenos.
-- True si el par esta marcado (y son distinguibles) y False si el par no esta marcado (y pueden ser equivalentes).
type TablaPares = Map.Map Par Bool

-- Mapa que nos servirá como la función delta para búsquedas mas rápidas.
type MapaDelta = Map.Map (Estado, Char) Estado

-- ########## Lógica para la minimización #########

-- ##### Función auxiliar para hacer que un ADF sea completo y regresar un mapa de transiciones #####
-- Para que sea completo, se añade un estado sumidero si es necesario.
completarAFD :: AFD -> (AFD, MapaDelta)
completarAFD afd =
  let
    estados = estadosD afd
    alfabeto = alfabetoD afd
    transiciones = transicionesD afd
    sumidero = "sumidero"
    -- Convertimos las transiciones en un Mapa para búsquedas.
    mapaDelta = Map.fromList [ ((q, c), dest) | (q, c, dest) <- transiciones ]
    -- Buscamos todas las transiciones faltantes.
    transFaltantes = [ (q, c) | q <- estados, c <- alfabeto, Map.notMember (q, c) mapaDelta ]
  in
    -- Si no falta ninguna, el AFD ya estaba completo.
    if null transFaltantes
    then (afd, mapaDelta)
    -- Si faltan, creamos el sumidero.
    else
      let
        -- Transiciones desde estados existentes hacia el sumidero.
        haciaSumidero = [ (q, c, sumidero) | (q, c) <- transFaltantes ]
        
        -- Transiciones del sumidero hacia sí mismo para cada símbolo.
        buclesSumidero = [ (sumidero, c, sumidero) | c <- alfabeto ]
        
        -- Creamos el nuevo AFD completo.
        afdCompleto = afd { 
            estadosD = sumidero : estados, 
            transicionesD = transiciones ++ haciaSumidero ++ buclesSumidero 
          }
          
        -- Creamos el nuevo mapa delta, que ahora estará completo.
        deltaCompleta = Map.union mapaDelta $ 
                        Map.fromList [ ((q, c), sumidero) | (q, c) <- transFaltantes ]
        deltaFinal = Map.union deltaCompleta $
                     Map.fromList [ ((sumidero, c), sumidero) | c <- alfabeto ]
                        
      in (afdCompleto, deltaFinal)


-- ##### Función auxiliar para ir marcando la tabla de pares de estados hasta que no haya cambios en la tabla #####
encontrarPuntoFijo :: MapaDelta -> [Char] -> TablaPares -> TablaPares
encontrarPuntoFijo mapaDelta alfabeto tablaActual =
  let

    -- Aplicamos la lógica de marcado a cada par en la tabla.
    tablaNueva = Map.mapWithKey (logicaDeMarcado tablaActual) tablaActual
    
    -- Si la nueva tabla es igual a la anterior, podemos brindar y decir que ya acabamos.
  in if tablaNueva == tablaActual
     then tablaActual
     -- Si no, repetimos el proceso con la nueva tabla
     else encontrarPuntoFijo mapaDelta alfabeto tablaNueva
  
  where
    -- Vamos a marcar la tablar
    logicaDeMarcado :: TablaPares -> Par -> Bool -> Bool
    logicaDeMarcado tabla (p, q) esMarcado
      -- Si ya está marcado, pues sigue marcado. XD.
      | esMarcado = True
      -- Si no está marcado, verificamos si tiene que marcarse.
      | otherwise =
          any (debeMarcarse (p, q)) alfabeto
    
    -- Para verificar si el par (p, q) debe marcarse.
    debeMarcarse :: Par -> Char -> Bool
    debeMarcarse (p, q) a =
      -- Vamos a ver los destinos de cada estado con "a".
      let destinoP = mapaDelta Map.! (p, a)
          destinoQ = mapaDelta Map.! (q, a)
      -- Vemos si el par de destninos esta marcado en la tabla.
      in esParDestinoMarcado (destinoP, destinoQ)

    -- Para verifica si un par de estados destino está marcado en la tabla actual.
    esParDestinoMarcado :: (Estado, Estado) -> Bool
    esParDestinoMarcado (p, q)
      -- Si transitan al mismo estado, no son distinguibles.
      | p == q = False
      -- Si no, buscamos su entrada en la tabla.
      | otherwise =
          -- Creamos el par ordenado para buscar en el Mapa.
          let (s1, s2) = if p < q then (p, q) else (q, p)
          -- Buscamos la marca del par destino.
          in tablaActual Map.! (s1, s2)
         

-- ##### Función auxiliar para agrupar los estados en particiones #####
encontrarParticiones :: [Estado] -> [Par] -> [Set.Set Estado]
encontrarParticiones estados paresEquivalentes =
  let
    -- Para cada par (p, q) agregamos q a la lista de p, y p a la lista de q.
    adjMap = Map.fromListWith (++) $ concat [ [(p, [q]), (q, [p])] | (p, q) <- paresEquivalentes ]
    -- Obtener los vecinos de un estado.
    vecinosDe q = Map.findWithDefault [] q adjMap

    -- Vamos a usar DFS para encontrar una partición, como si estuviéramos en una gráfica.
    dfs :: Estado -> Set.Set Estado -> (Set.Set Estado, Set.Set Estado)
    dfs nodo visitados =
      -- Marcamos el 'nodo' actual como visitado.
      let nuevosVisitados = Set.insert nodo visitados
          -- Encontramos sus vecinos que no hemos visitado.
          vecinos = filter (`Set.notMember` nuevosVisitados) (vecinosDe nodo)
          -- Recorremos recursivamente a todos los vecinos.
          (particionVecinos, visitadosFinal) =
            foldl (\(acumParticion, acumVisitados) vecino ->
                      let (subParticion, vis) = dfs vecino acumVisitados
                      in (Set.union acumParticion subParticion, vis))
                  (Set.empty, nuevosVisitados)
                  vecinos
                  
      -- La partición final es el nodo más las particiones de sus vecinos.
      in (Set.insert nodo particionVecinos, visitadosFinal)

    -- Función para recorrer todos los estados y encontrar todas las particiones.
    buscar :: [Estado] -> Set.Set Estado -> [Set.Set Estado]
    -- Caso base: no hay estados.
    buscar [] _ = []
    buscar (s:ss) visitados
      -- Si ya visitamos este estado, esta en una partición que ya encontramos.
      | s `Set.member` visitados = buscar ss visitados
      -- Si no, es una nueva partición. La exploramos con nuestra implementación de dfs.
      | otherwise =
          let (nuevaParticion, visitadosActualizados) = dfs s visitados
          -- Agregamos la nueva partición a la lista y seguimos.
          in nuevaParticion : buscar ss visitadosActualizados
          
  -- Iniciamos la búsqueda desde el primer estado con un conjunto de visitados vacío.
  in buscar estados Set.empty



-- >>>> Función principal que toma un AFD y regresa un AFDmin <<<<
minimizaAFD :: AFD -> AFD
minimizaAFD afd =
    -- Primero hay que completar el autómata, pues esta estrategia asume que todo estado 
    -- tiene una transición con el alfabeto. 
    let (afdCompleto, mapaDelta) = completarAFD afd     
        -- Extraemos los elementos del AFD completo para trabajar con ellos.
        estados = estadosD afdCompleto
        alfabeto = alfabetoD afdCompleto
        -- Que mejor sea Set para hacer búsquedas mas rápidas.
        finales = Set.fromList (finalesD afdCompleto)
        inicial = inicialD afdCompleto
    
        -- Vamos a iniciar la tabla. Primero tenemos que Crear la lista de todos los pares (p, q)
        todosPares = [ (q1, q2) | q1 <- estados, q2 <- estados, q1 < q2 ]
    
        -- Ahora creamos la tabla inicial donde marcamos como true a todos los pares
        -- donde uno es final y el otro no.
        tablaInicial = Map.fromList [ (p, esMarcadoInicial p) | p <- todosPares ]
          where
            esFinal q = q `Set.member` finales
            -- Un xor entre los estados.
            esMarcadoInicial (p, q) = esFinal p /= esFinal q

        -- Marcaremos hasta que la tabla ya no cambie.
        tablaFinal = encontrarPuntoFijo mapaDelta alfabeto tablaInicial

        -- Los pares que no están marcados, que tienen False, son equivalentes.
        paresEquivalentes = Map.keys $ Map.filter (== False) tablaFinal
    
        -- Agrupamos los estados en particiones.
        particiones = encontrarParticiones estados paresEquivalentes

        -- Hacemos tro mapa 'Estado -> Partición' para saber a quq nuevo conjunto pertenece cada estado antiguo.
        mapaEstadoAParticion = Map.fromList [ (q, p) | p <- particiones, q <- Set.toList p ]
        -- Se encuentra la partición de un estado
        particionDe q = mapaEstadoAParticion Map.! q

        -- El nombre de un nuevo estado es el nombre de su partición.
        renombraParticion = renombra . sort . Set.toList
    
        -- Los estados del nuevo AFD son los nombres de las particiones.
        nuevosEstados = map renombraParticion particiones
    
        -- El nuevo estado inicial es la partición que contiene al inicial original.
        nuevoInicial = renombraParticion (particionDe inicial)
    
        -- Los nuevos finales son las particiones que contienen cualquier final original.
        nuevosFinales = nub [ renombraParticion p | p <- particiones, any (`Set.member` finales) (Set.toList p) ]
                   
        -- Las nuevas transiciones se crean desde cada partición.
        nuevasTrans = [ (renombraParticion p, c, renombraParticion (particionDe destino))
                      | p <- particiones,
                        c <- alfabeto,
                        let representante = Set.findMin p,
                        let destino = mapaDelta Map.! (representante, c)
                      ]

    -- Finalmente, armamos todo.
    in AFD { estadosD = nuevosEstados,
             alfabetoD = alfabeto,
             transicionesD = nuevasTrans,
             inicialD = nuevoInicial,
             finalesD = nuevosFinales }
