module Gramatica where

import ER    -- importa tu módulo ER.hs, que a su vez debería importar Expr.hs

-- Gramatica/lenguaje IMP (Tokens)
data Expr = Term Char | Or Expr Expr | And Expr Expr | Kleene Expr | Range Char Char

-- Palabras Reservadas


-- Identificadores
letraMayus, letraMinus, digito :: Expr
letraMayus = Range 'A' 'Z'
letraMinus = Range 'a' 'z'
digito = Range '0' '9'

identificador :: Expr
identificador =  And (Or letraMayus letraMinus) (Kleene (Or (Or letraMayus letraMinus) digito)) -- letra(letra+digito)*

-- Enteros
enteros :: Expr
enteros = Or (Term '0') (Or (And (Range '1' '9')(Kleene digito))(And (Term '-')(And (Range '1' '9')(Kleene digito)))) -- 0 | ([1-9][0-9]*) | -([1-9][0-9]*)

-- Operadores
operadores :: Expr
operadores = Or (Or (Or (Term '+')(Term '-'))(Term '*'))(Or (Or(Term '=')(And(Term '<')(Term '=')))(And(Term ':')(Term '='))) -- + | - | * | = | <= | :=
--duda, si quiere que funcione para casi todos los lenguajes, no solo para IMP entonces también deberia de estar /, pero entonces ya no solo aceptaria (o se definiria los enteros sino tambien los reales)

-- Delimitadores
delimitadores :: Expr
delimitadores = Term ';' -- ;, no se si tambien los ()

-- Espacios
espacios :: Expr
espacios = Kleene (Term ' ') -- ' '*

-- Comentarios

