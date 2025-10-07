module Gramatica where

import ER

-- Gramatica/lenguaje IMP (Tokens)
--data Expr = Term Char | Or Expr Expr | And Expr Expr | Kleene Expr | Range Char Char

-- Palabras Reservadas
skip_ :: Expr
skip_ = And (Term 's') (And (Term 'k') (And (Term 'i') (Term 'p'))) -- skip
if_ :: Expr
if_ = And (Term 'i') (Term 'f') -- if
then_ :: Expr
then_ = And (Term 't') (And (Term 'h') (And (Term 'e') (Term 'n'))) -- then
else_ :: Expr
else_ = And (Term 'e') (And (Term 'l') (And (Term 's') (Term 'e'))) -- else
while_ :: Expr
while_ = And (Term 'w') (And (Term 'h') (And (Term 'i') (And (Term 'l') (Term 'e')))) -- while
do_ :: Expr
do_ = And (Term 'd') (Term 'o') -- do
true_ :: Expr
true_ = And (Term 't') (And (Term 'r') (And (Term 'u') (Term 'e'))) -- true
false_ :: Expr
false_ = And (Term 'f') (And (Term 'a') (And (Term 'l') (And (Term 's') (Term 'e')))) -- false
not_ :: Expr
not_ = And (Term 'n') (And (Term 'o') (Term 't')) -- not
and_ :: Expr
and_ = And (Term 'a') (And (Term 'n') (Term 'd')) -- and

palabrasReservadas :: Expr
palabrasReservadas = Or skip_ (Or if_ (Or then_ (Or else_ (Or while_ (Or do_ (Or true_ (Or false_ (Or not_ and_))))))))-- skip | if | then | else | while | do | true | false | not | and

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
comentarios :: Expr
comentarios = And (Term '-') (And (Term '-') (Kleene (Range ' ' '~'))) -- --[ -~]*, la ~ indica que va desde el rango de un espacio hasta el ultimo caracter en ascii para asi no tener que escribir todos los caracteres (que el comentario aceptaria) explicitos.
