import           Data.List ((\\))

data Exp
  = Let String Exp Exp      -- let x = e1 in e2
  -- | Fun String [String] Exp -- def f(x1, x2, ..., x_n): ...
  | Exp1 Exp1
  deriving (Show)

data Exp1
  = Plus Exp1 Term          -- e + t
  | Minus Exp1 Term         -- e - t
  | Term Term               -- t
  deriving (Show)

data Term
  = Times Term Factor       -- t * f
  | Div Term Factor         -- t / f
  | Factor Factor           -- f
  deriving (Show)

data Factor
  = Int Int
  | Var String
  | Brack Exp
  deriving (Show)


-- let z = 4 in x * 3 - (1 - y) + z
example1 :: Exp
example1 =
    Let "z" (Exp1 (Term (Factor (Int 4))))
        (Exp1 (Plus
        (Minus
            (Term (Times (Factor (Var "x")) (Int 3)))
            (Factor (Brack (Exp1 (Minus (Term (Factor (Int 1))) (Factor (Var "y"))))))
        )
        (Factor (Var "z"))
        ))

-- # free vars in the body of f: z
-- def f(x, y):
--   return z + x + y     # z, x, y

freeVars :: Exp -> [String]
freeVars exp =
  case exp of
    Let x e1 e2 -> freeVars e1 ++ (freeVars e2 \\ [x])
    Exp1 exp1   -> freeVarsExp1 exp1

freeVarsExp1 :: Exp1 -> [String]
freeVarsExp1 exp1 =
  case exp1 of
    Plus e t  -> freeVarsExp1 e ++ freeVarsTerm t
    Minus e t -> freeVarsExp1 e ++ freeVarsTerm t
    Term t    -> freeVarsTerm t

freeVarsTerm :: Term -> [String]
freeVarsTerm t =
  case t of
    Times t f -> freeVarsTerm t ++ freeVarsFactor f
    Div t f   -> freeVarsTerm t ++ freeVarsFactor f
    Factor f  -> freeVarsFactor f

freeVarsFactor :: Factor -> [String]
freeVarsFactor f =
  case f of
    Var x   -> [x]
    Int _   -> []
    Brack e -> freeVars e

main :: IO()
main = print (freeVars example1)