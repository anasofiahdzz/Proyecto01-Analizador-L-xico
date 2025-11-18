-- >>>>> Definición de AFDmin y la función para pasar de AFD --> AFDmin <<<<<
module AFDmin where

-- Importamos la definición de AFD y el tipo Estado
import AFD
import AFNEp (Estado)

-- Usaremos Map y Set para una implementación eficiente de la tabla y de los conjuntos de estados.
import qualified Data.Map.Strict as Mapa
import qualified Data.Set as Conj
-- find para buscar y sort para renombrar.
import Data.List (sort, find, nub, intercalate)

-- **** Nuevos tipos auxiliares ****

-- Par de estados (p, q) donde p < q. Esto para usarlo en un Mapa.
type Par = (Estado, Estado)

-- Implementación de la tabla que debemos ir marcando como lo vimos en clase. La vamos a ir mapeando con boolenos.
-- True si el par esta marcado (y son distinguibles) y False si el par no esta marcado (y pueden ser equivalentes).
type TablaPares = Mapa.Map Par Bool

-- Mapa que nos servirá como la función delta para búsquedas mas rápidas. Sería como una especie de 
-- cache para no tener que buscar en la lista de transiciones cada vez.
type MapaDelta = Mapa.Map (Estado, Char) Estado



-- ########## Lógica para la minimización #########

-- === (1) Funciones para ir marcando la tabla de pares de estados hasta que no haya cambios en la tabla ===



-- \\\ Función que verifica si un par de estados está marcado en la tabla actual ///
esParMarcado :: TablaPares -> (Estado, Estado) -> Bool
esParMarcado tablaActual (p, q)
  -- Si transitan al mismo estado, no son distinguibles.
  | p == q = False
  -- Si no, vamos a buscar su entrada en la tabla.
  | otherwise =
      -- Creamos el par ordenado para buscar en el Mapa.
      let (s1, s2) = if p < q then (p, q) else (q, p)
      -- Buscamos la marca del par destino.
      in tablaActual Mapa.! (s1, s2)
      
      
      
-- \\\ Función que verifica si el par (p, q) debe marcarse por el símbolo 'a' ///
debeMarcarse :: TablaPares -> MapaDelta -> Par -> Char -> Bool
debeMarcarse tablaActual mapaDelta (p, q) a =
  -- Vamos a ver la delta de cada estado con "a".
  let deltaP = mapaDelta Mapa.! (p, a)
      deltaQ = mapaDelta Mapa.! (q, a)
  -- Vemos si el par de destinos esta marcado en la tabla.
  in esParMarcado tablaActual (deltaP, deltaQ)      
      
      

-- \\\ Función para marcar un par de la tabla ///
logicaDeMarcado :: TablaPares -> MapaDelta -> [Char] -> Par -> Bool -> Bool
logicaDeMarcado tablaActual mapaDelta alfabeto (p, q) esMarcado
  -- Si ya está marcado, pues ya acabamos.
  | esMarcado = True
  -- Si no está marcado, verificamos si tiene que marcarse.
  | otherwise =
      -- Verificamos si cualquier símbolo del alfabeto nos obliga a marcar este par.
      any (debeMarcarse tablaActual mapaDelta (p, q)) alfabeto
      

  
-- \\\ Función para ir marcando la tabla de pares de estados hasta que no haya cambios en la tabla ///
encontrarPunto :: MapaDelta -> [Char] -> TablaPares -> TablaPares
encontrarPunto mapaDelta alfabeto tablaActual =
  let
    -- Aplicamos la lógica de marcado a cada par en la tabla.
    tablaNueva = Mapa.mapWithKey (\par bool -> logicaDeMarcado tablaActual mapaDelta alfabeto par bool) tablaActual
    
  -- Si la nueva tabla es igual a la anterior, podemos brindar y decir que ya acabamos.
  in if tablaNueva == tablaActual
     then tablaActual
     -- Si no, repetimos el proceso con la nueva tabla
     else encontrarPunto mapaDelta alfabeto tablaNueva      
      
      
      
-- === (2) Encontrar particiones ===



-- \\\ Función que obtiene los estados equivalentes de un estado  ///
vecinosDe :: Mapa.Map Estado [Estado] -> Estado -> [Estado]
vecinosDe mapaA q = Mapa.findWithDefault [] q mapaA



-- \\\ Función DFS para encontrar un una partición ///
dfs :: Mapa.Map Estado [Estado] -> Estado -> Conj.Set Estado -> (Conj.Set Estado, Conj.Set Estado)
dfs mapaA q visitados =
  -- Marcamos el nodo 'q' actual como visitado.
  let nuevosVisitados = Conj.insert q visitados
      -- Encontramos sus vecinos que no hemos visitado.
      vecinos = filter (`Conj.notMember` nuevosVisitados) (vecinosDe mapaA q)
      -- Recorremos recursivamente a todos los vecinos.
      (particionVecinos, visitadosFinal) =
        foldl (\(acumParticion, acumVisitados) vecino ->
                  let (subParticion, vis) = dfs mapaA vecino acumVisitados
                  in (Conj.union acumParticion subParticion, vis)) (Conj.empty, nuevosVisitados) vecinos
              
  -- La partición final es el estado más las particiones de sus vecinos.
  in (Conj.insert q particionVecinos, visitadosFinal)
  
  
  
-- \\\ Función que recorre todos los estados y encuentra todas las particiones ///
buscar :: Mapa.Map Estado [Estado] -> [Estado] -> Conj.Set Estado -> [Conj.Set Estado]
-- Caso base: no hay estados que reccorrer.
buscar _ [] _ = []
buscar mapaA (q : qs) visitados
  -- Si ya visitamos este estado, esta en una partición que ya encontramos.
  | q `Conj.member` visitados = buscar mapaA qs visitados
  -- Si no, es una nueva partición. La exploramos con dfs.
  | otherwise =
      let (nuevaParticion, visitadosActualizados) = dfs mapaA q visitados
      -- Agregamos la nueva partición a la lista y seguimos.
      in nuevaParticion : buscar mapaA qs visitadosActualizados
   


-- \\\ Función para agrupar los estados en particiones ///
encontrarParticiones :: [Estado] -> [Par] -> [Conj.Set Estado]
encontrarParticiones estados paresEquivalentes =
  let
    -- Hacemos el mapa de adyacencia.
    mapaA = Mapa.fromListWith (++) $ concat [ [(p, [q]), (q, [p])] | (p, q) <- paresEquivalentes ]
  -- Iniciamos la búsqueda con el mapa que acabamos de hacer.
  in buscar mapaA estados Conj.empty


-- === Pasar de AFD a AFD min ===

-- \\\ Función para renombrar particiones ///
renombraP :: [Estado] -> Estado
renombraP = intercalate "," . sort



-- \\\ Función principal que toma un AFD y regresa un AFDmin ///
minimizaAFD :: AFD -> AFD
minimizaAFD afd =
        -- Solo necesitamos construir el MapaDelta para búsquedas rápidas.
    let mapaDelta = Mapa.fromList [ ((q, c), dest) | (q, c, dest) <- transicionesD afd ]
        -- Obtenemos los elementos del AFD completo para trabajar con ellos.
        estados = estadosD afd
        alfabeto = alfabetoD afd
        finales = Conj.fromList (finalesD afd)
        inicial = inicialD afd
    
        -- Vamos a iniciar la tabla. Primero tenemos que crear la lista de todos los pares (p, q)
        todosPares = [ (q1, q2) | q1 <- estados, q2 <- estados, q1 < q2 ]
    
        -- Ahora creamos la tabla inicial donde marcamos como true a todos los pares
        tablaInicial = Mapa.fromList [ (p, esMarcadoInicial p) | p <- todosPares ]
          where
            esFinal q = q `Conj.member` finales
            -- Xor entre los estados.
            esMarcadoInicial (p, q) = esFinal p /= esFinal q

        -- Marcaremos hasta que la tabla ya no cambie.
        tablaFinal = encontrarPunto mapaDelta alfabeto tablaInicial

        -- Los pares que no están marcados (false) son equivalentes.
        paresEquivalentes = Mapa.keys $ Mapa.filter (== False) tablaFinal
    
        -- Agrupamos los estados en particiones.
        particiones = encontrarParticiones estados paresEquivalentes

        -- Hacemos otro mapa 'Estado -> Partición' para saber a que nuevo conjunto pertenece cada estado antiguo.
        mapaEstadoAParticion = Mapa.fromList [ (q, p) | p <- particiones, q <- Conj.toList p ]
        -- Se encuentra la partición de un estado
        particionDe q = mapaEstadoAParticion Mapa.! q

        -- El nombre de un nuevo estado es el nombre de su partición.
        renombraParticion = renombraP . Conj.toList
    
        -- Los estados del nuevo AFD son los nombres de las particiones.
        nuevosEstados = map renombraParticion particiones
    
        -- El nuevo estado inicial es la partición que contiene al inicial original.
        nuevoInicial = renombraParticion (particionDe inicial)
    
        -- Los nuevos finales son las particiones que contienen cualquier final original.
        nuevosFinales = nub [ renombraParticion p | p <- particiones, any (`Conj.member` finales) (Conj.toList p) ]
                   
        -- Las nuevas transiciones se crean desde cada partición.
        nuevasTrans = [ (renombraParticion p, c, renombraParticion (particionDe destino))
                      | p <- particiones,
                        c <- alfabeto,
                        let representante = Conj.findMin p,
                        let destino = mapaDelta Mapa.! (representante, c)
                      ]
        
          -- Armamos todo.
        afdMinIntermedio = AFD { estadosD = nuevosEstados,
                                     alfabetoD = alfabeto,
                                     transicionesD = nuevasTrans,
                                     inicialD = nuevoInicial,
                                     finalesD = nuevosFinales }

    -- Finalmente, renombramos los
    in renombrarAFD afdMinIntermedio
