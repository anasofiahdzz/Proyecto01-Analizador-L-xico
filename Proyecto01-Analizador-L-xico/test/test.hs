module Main where

import Test.QuickCheck
import Data.List (groupBy, sortOn)
import Data.Function (on)
import ER
import AFNEp
import AFN
import AFD
import Gramatica

-- Ejecuta solo 1 prueba por propiedad,sin generar muchos casos aleatorios.
qc1 :: Testable prop => prop -> IO ()
qc1 = quickCheckWith stdArgs { maxSuccess = 1 }

-- ---------- EJEMPLOS PARA LAS PRUEBAS ----------------
-- Definimos dos expresiones regulares de ejemplo
-- Caso 1 - Expresión Regular: (a+b)*c
exprEjemplo1 :: Expr
exprEjemplo1 = And (Kleene (Or (Term 'a') (Term 'b'))) (Term 'c')

-- Caso 2 - Expresión Regular: [1-3], cualquier dígito entre 1 y 3
exprEjemplo2 :: Expr
exprEjemplo2 = Range '1' '3'

-- ----------- FUNCIONES DE VERIFICACIÓN -----------------

-- Verificamos que un AFNε esté bien construido
afnepBienFormado :: AFNEp -> Bool
afnepBienFormado afnEpsilon =
  let listaEstados  = estados afnEpsilon -- Extrae la lista de estados del autómata m y la guarda en qs para usarla varias veces.
      transicionValida (estadoOrigen, simboloTalVez, estadosDestino) =
        estadoOrigen `elem` listaEstados -- Revisa si la transición es válida
        && all (`elem` listaEstados) estadosDestino
        && case simboloTalVez of
             Nothing         -> True
             Just simbolo    -> simbolo `elem` alfabeto afnEpsilon
  in inicial afnEpsilon `elem` listaEstados -- El estado inicial debe estar en la lista de estados
     && final afnEpsilon `elem` listaEstados -- El estado final debe estar en la lista de estados
     && all transicionValida (transiciones afnEpsilon) -- Todas las transiciones deben ser válidas

-- Verificamos que un AFN ya no tenga transiciones epsilon
sinEpsilonEnAFN :: AFN -> Bool
sinEpsilonEnAFN afn =
  let simbolosAlfabeto = alfabetoN afn
  in all (\(_, simbolo, _) -> simbolo `elem` simbolosAlfabeto)
         (transicionesN afn) -- No debe haber transiciones con epsilon

-- Verifica que los estados finales del AFN (sin epsilon), se obtienen por cerradura.
finalesPorCerradura :: AFNEp -> AFN -> Bool
finalesPorCerradura afnEpsilon afn =
  let finalesCalculados =
        [ estadoActual
        | estadoActual <- estados afnEpsilon
        , final afnEpsilon
            `elem` eclosure (transiciones afnEpsilon)
                            afnEpsilon
                            estadoActual
        ]
  in sortOn id finalesCalculados == sortOn id (finalesN afn) -- Compara y ordena los estados finales calculados con los del AFN

-- Verifica que el AFD sea determinista: a lo más una transición por (estado, símbolo)
esDeterminista :: AFD -> Bool
esDeterminista afd =
  let clave (estadoOrigen, simbolo, _) = (estadoOrigen, simbolo)
  -- Agrupa las transiciones por (estado,símbolo) y verifica que cada grupo tenga a lo más una transición
      grupos = groupBy ((==) `on` clave) . sortOn clave $ transicionesD afd
  in all ((<= 1) . length) grupos

-- Verifica que el estado inicial del AFD se haya construido correctamente a partir del AFN.
inicialCorrecto :: AFN -> AFD -> Bool
inicialCorrecto afn afd =
  let estadoInicialAFN = inicialN afn
  in estadoInicialAFN `elem` separarEstados (inicialD afd) -- el estado inicial del AFD debe contener el estado inicial del AFN
  where
    separarEstados "" = []
    separarEstados cadenaEstados =
      case break (==',') cadenaEstados of
        (headEstado, "")     -> [headEstado]
        (headEstado, _:rest) -> headEstado : separarEstados rest
        -- separa una cadena de estados separados por comas en una lista de estados individuales

--Verifica que los estados finales del AFD se hayan construido correctamente a partir del AFN.
finalesCorrectos :: AFN -> AFD -> Bool
finalesCorrectos afn afd =
  let esEstadoFinal nombreEstadoAFD =
        any (`elem` separarEstados nombreEstadoAFD) (finalesN afn)
  in all esEstadoFinal (finalesD afd) -- cada estado final del AFD debe contener al menos un estado final del AFN
  where
    separarEstados "" = []
    separarEstados cadenaEstados =
      case break (==',') cadenaEstados of
        (headEstado, "")     -> [headEstado]
        (headEstado, _:rest) -> headEstado : separarEstados rest
        -- separa una cadena de estados separados por comas en una lista de estados individuales

-- ------------------- PRUEBAS POR CASOS FIJOS ----------------------

-- Pruebas de Expresiones Regulares
pruebasER :: IO ()
pruebasER = do
  putStrLn "== Pruebas de Expresiones Regulares =="
  -- Verifica que showTerm sea exactamente el carácter entre comillas.
  qc1 (show (Term 'a') == "a")
  qc1 (show (Term '1') == "1")
  -- Rango [1-3] genera tantas transiciones como elementos del rango
  qc1 $ let (afnEpsilon, _) = expr_to_AFNEp (Range '1' '3') 0
        in length (transiciones afnEpsilon) == length ['1'..'3']
  -- Rango [a-c] genera tantas transiciones como elementos del rango
  qc1 $ let (afnEpsilon, _) = expr_to_AFNEp (Range 'a' 'c') 0
        in length (transiciones afnEpsilon) == length ['a'..'c']

-- ER → AFNε → AFN → AFD, y valida propiedades clave en cada etapa.
pruebasConExpr :: String -> Expr -> IO ()
pruebasConExpr nombreCaso expresion = do
  putStrLn $ "\n== Caso de prueba: " ++ nombreCaso ++ " =="
  -- AFNε
  let (afnEpsilon, _) = expr_to_AFNEp expresion 0
  qc1 (afnepBienFormado afnEpsilon)
  -- AFN (sin epsilon)
  let afn = afnEp_to_AFN afnEpsilon
  qc1 (sinEpsilonEnAFN afn)
  qc1 (finalesPorCerradura afnEpsilon afn)
  -- AFD
  let afd = afn_to_afd afn
  qc1 (esDeterminista afd)
  qc1 (inicialCorrecto afn afd)
  qc1 (finalesCorrectos afn afd)

-- ----Main----
main :: IO ()
main = do
  putStrLn "=== Pruebas deterministas (casos fijos) ==="
  pruebasER
  pruebasConExpr "(a+b)*c" exprEjemplo1
  pruebasConExpr "[1-3]"   exprEjemplo2
