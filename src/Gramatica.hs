-- >>>>> Representación de nuestra gramática <<<<<
module Gramatica where

-- Importamos nuestra ER.
import ER

-- ##### Gramática del lenguaje IMP #####

-- <<< Palabras Reservadas >>> 

-- ER's auxiliares para poner tener tal cual las palabras reservadas.
true_ :: Expr
true_ = And (Term 't') (And (Term 'r') (And (Term 'u') (Term 'e'))) -- true.

false_ :: Expr
false_ = And (Term 'f') (And (Term 'a') (And (Term 'l') (And (Term 's') (Term 'e')))) -- false.

not_ :: Expr
not_ = And (Term 'n') (And (Term 'o') (Term 't')) -- not.

and_ :: Expr
and_ = And (Term 'a') (And (Term 'n') (Term 'd')) -- and.

skip_ :: Expr
skip_ = And (Term 's') (And (Term 'k') (And (Term 'i') (Term 'p'))) -- skip.

if_ :: Expr
if_ = And (Term 'i') (Term 'f') -- if.

then_ :: Expr
then_ = And (Term 't') (And (Term 'h') (And (Term 'e') (Term 'n'))) -- then.

else_ :: Expr
else_ = And (Term 'e') (And (Term 'l') (And (Term 's') (Term 'e'))) -- else.

while_ :: Expr
while_ = And (Term 'w') (And (Term 'h') (And (Term 'i') (And (Term 'l') (Term 'e')))) -- while.

do_ :: Expr
do_ = And (Term 'd') (Term 'o') -- do.

--  --->     ER de las palabras reservadas ::  skip | if | then | else | while | do | true | false | not | and         <---
palabrasReservadas :: Expr
palabrasReservadas = Or true_ (Or false_ (Or not_ (Or and_ (Or skip_ (Or if_ (Or then_ (Or else_ (Or while_ do_)))))))) 



-- <<< Identificadores >>>

-- ER's auxiliares para todas las letras y los dígitos.
letraMayus, letraMinus, digito, digitoSinC :: Expr
letraMayus = Range 'A' 'Z' -- Todas las letras mayúsculas.
letraMinus = Range 'a' 'z' -- Todas las letras minúsculas.
digito = Range '0' '9' -- Números del 0 al 9.
digitoSinC = Range '1' '9' -- Números del 1 al 9.

-- --->     ER de los IDs :: letra(letra + dígito)^*     <---
identificador :: Expr
identificador =  And (Or letraMayus letraMinus) (Kleene (Or (Or letraMayus letraMinus) digito))
  
  
  
-- <<< Enteros >>>

-- --->     ER de los enteros :: 0 | ([1-9][0-9]^*) | -([1-9][0-9]^*)     <---
enteros :: Expr
enteros = Or (Term '0') (Or (And (digitoSinC)(Kleene digito))(And (Term '-')(And (digitoSinC)(Kleene digito))))



-- <<< Operadores >>>

-- --->     ER de los operadores :: + | - | * | = | <= | :=     <---
operadores :: Expr
operadores = Or (Or (Or (Term '+')(Term '-'))(Term '*'))(Or (Or(Term '=')(And(Term '<')(Term '=')))(And(Term ':')(Term '='))) 



-- <<< Delimitadores >>>

-- --->     ER de los delimitadores :: ; | ( | )     <---
delimitadores :: Expr
delimitadores =   Or (Term ';')(Or (Term '(') (Term ')'))


-- <<< Espacios >>>

-- ER auxiliar para los espacios en blanco.
espacioBlanco :: Expr
espacioBlanco = Or (Term ' ') (Or (Term '\t') (Term '\n') ) -- (' ' + \t + \n)

-- --->     ER de los espacios en blanco :: (' ' + \t + \n)(' ' + \t + \n)^*     <---
espacios :: Expr
espacios = And espacioBlanco (Kleene espacioBlanco)



-- <<< Comentarios >>>

-- ER auxiliar para representar todo el alfabeto de IMP.
alfabeto :: Expr
alfabeto = Range ' ' '~'

-- --->     ER de los comentarios :: // (alfabeto)^* \n     <---
comentarios :: Expr
comentarios = And (Term '/') (And (Term '/') (And (Kleene alfabeto) (Term '\n'))) 
