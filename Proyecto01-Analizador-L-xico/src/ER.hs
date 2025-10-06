module ER where

-- Abstracción de Expresiones Regulares
data Expr
  = Term Char          -- símbolo terminal
  | And Expr Expr      -- concatenación
  | Or Expr Expr       -- unión
  | Kleene Expr        -- ciExprre de Kleene
  deriving (Eq)

instance Show Expr where
  show (Term c)   = [c]
  show (And l r)  = "(" ++ show l ++ show r ++ ")"
  show (Or l r)   = "(" ++ show l ++ "+" ++ show r ++ ")"
  show (Kleene e) = show e ++ "*"

-- | Abstracción de una transición epsilón,
-- utilizamos Nothing para modelar una transición epsilón.
-- la tercia representa la función delta, el primero es
-- el estado donde se lee el cáracter o el epsilon y te lleva
-- a una lista de estados (no determinista).
type Trans_eps = (String, Maybe Char, [String])
