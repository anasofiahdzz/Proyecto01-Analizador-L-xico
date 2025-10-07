module ER where
type Trans_eps = (String, Maybe Char, [String])

-- Abstracción de Expresiones Regulares
--data Expr = Term Char | Or Expr Expr | And Expr Expr | Kleene Expr | Range Char Char  deriving (Show, Eq)
data Expr = Term Char | Or Expr Expr | And Expr Expr | Kleene Expr | Range Char Char
  deriving (Eq)


-- Aqui se convierte una expresion regular del tipo Expr a una cadena de texto, usando Show
instance Show Expr where
  show (Term c)   = [c] --termino c pasa a solo mostrar c
  show (And l r)  = "(" ++ show l ++ show r ++ ")" -- cuando es l y r, se muestra la concatenacion : (lr)
  show (Or l r)   = "(" ++ show l ++ "+" ++ show r ++ ")"  -- cuando es l o r, s muestra como (l + r)
  show (Kleene e) = show e ++ "*" -- cuando se usa la estrella se muestra como una expr*
  show (Range l r)= "[" ++ [l] ++ "-" ++ [r] ++ "]" -- cuando es un rango se muestra como [l-r]