module AFD where
import AFN

-- Transición determinista
type Trans_afd = (String, Char, String) -- (estadoOrigenDet, simboloLeido, estadoDestinoDet)

-- Definición de AFD
data AFD = AFD{ estadosD :: [String], alfabetoD :: [Char],
                transicionesD :: [Trans_afd],
                inicialD :: String, finalesD :: [String]} deriving (Show)

--Convierte una lista de estados en un nombre único separado por comas: ["q0","q1"] -> "q0,q1"
renombra :: [String] -> String
renombra xs = concat (separa xs) -- une la lista que devuelve "separa"
  where
    separa[]= [] --Caso base
    separa[y]= [y] --Ultimo elemento(no se pone una coma)
    separa(y:ys)= y : [","] ++ separa ys --Pega y sigue con el resto

-- Construcción por subconjuntos de un AFD a partir de un AFN
afn_to_afd :: AFN -> AFD
afn_to_afd afn = AFD{ estadosD = map snd todosEstados, alfabetoD = alfabetoN afn,
                    transicionesD = todasTrans,
                    inicialD = inicialStr, finalesD = finalesDets}
  where
    -- conjunto que contiene el estado inicial del AFN
    inicialSet = [inicialN afn]
    inicialStr = renombra inicialSet -- renombra del estado inicial del AFD

    -- Procesamos estados pendientes en una lista para descubrir nuevos subconjuntos
    procesa :: [([String], String)] -- (lista, nombre)
            -> [([String], String)] -- ya procesados
            -> [([String], String)] -- resultado final
    procesa [] visitados = visitados
    procesa ((conj, nombre):pendientes) visitados
      | nombre `elem` map snd visitados = procesa pendientes visitados -- si ya fue visitado, lo saltamos
      -- si no, lo marcamos como visitado y agregamos los nuevos estados encontrados
      | otherwise = procesa (pendientes ++ nuevos) (visitados ++ [(conj, nombre)]) 
      where
        -- construye los nuevos estados destino para cada símbolo válido.
        nuevos = [ (alcanzables conj c, renombra (alcanzables conj c))
                 | c <- alfabetoN afn
                 , not (null (alcanzables conj c))
                 , renombra (alcanzables conj c) `notElem` map snd visitados
                 ]

    --obtenemos todos los estados a los que puedo llegar desde un subconjunto.
    alcanzables :: [String] -> Char -> [String]
    alcanzables conj c =
      rmDup [ d | (q, c', ds) <- transicionesN afn -- recorre cada transición del AFN
            , q `elem` conj
            , c' == c
            , d <- ds -- tomamos cada destino de la lista ds
            ]

    -- Lista de todos los estados deterministas.
    todosEstados = procesa [(inicialSet, inicialStr)] []

    --Obtenemos TODAS las transiciones por cada estado determinista y símbolo
    todasTrans = 
      [ (nombre, c, renombra (alcanzables conjunto c))
      | (conjunto, nombre) <- todosEstados
      , c <- alfabetoN afn
      , not (null (alcanzables conjunto c))
      ]

    --Obtenemos todos los estados que dentro de su conjuto tiene a alguno final del AFN (Finales del AFD)
    finalesDets =
      [ nombre
      | (conjunto, nombre) <- todosEstados
      , any (`elem` finalesN afn) conjunto
      ]
