-- >>>>> Representación de una expresión regular <<<<<
module ER where

-- *** Abstracción de Expresiones Regulares ***
data Expr = Term Char | Or Expr Expr | And Expr Expr | Kleene Expr | Range Char Char
  deriving (Eq)

-- *** Se convierte una expresión regular del tipo Expr a String, usando Show
instance Show Expr where
  show (Term c)   = [c] --termino c pasa a solo mostrar c
  show (And l r)  = "(" ++ show l ++ show r ++ ")" -- cuando es l y r, se muestra la concatenación : (lr)
  show (Or l r)   = "(" ++ show l ++ "+" ++ show r ++ ")"  -- cuando es l o r, s muestra como (l + r)
  show (Kleene e) = show e ++ "*" -- cuando se usa la estrella se muestra como una expr*
  show (Range l r)= "[" ++ [l] ++ "-" ++ [r] ++ "]" -- cuando es un rango se muestra como [l-r]
