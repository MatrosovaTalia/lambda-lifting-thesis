
import Data.List ((\\))
import Text.Show



data Fun 
    = Exp
    | Identifier Parameters Body
    deriving (Show)


-- data FunCall Parameters
--     = Exp
--     deriving (Show)

data Identifier
    = String
    deriving (Show)

data Statement 
    = IfStatement
    | Return
    | WhileLoop
    | ForLoop
    deriving (Show)

data IfStatement 
    = Condition IfBody ElseBody
    | IfBody
    deriving (Show)

data IfBody
    = Body
    deriving (Show)

data Return
    = Exp
    deriving (Show)

data WhileLoop
    = Exp
    deriving (Show)

data 

data Return Exp
    = Exp
    deriving (Show)

data WhileLoop Condition Body
    = Exp
    deriving (Show)

data ForLoop Condition Body
    = Exp
    deriving (Show)

data Condition
    = BoolExp
    deriving (Show)

data Body 
    = [Exp]
    deriving (Show)

data Parameters
    = [Identifier]
    deriving (Show) 

data Exp
    = Let Identifier Exp Exp      -- let x = e1 in e2
    | Summand Summand
    deriving (Show)

data Summand
    = Plus Summand Term          -- e + t
    | Minus Summand Term         -- e - t
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

data Identifier
    = String
    deriving (Show)


-- let z = 4 in x * 3 - (1 - y) + z
example1 :: Exp
example1 =
  Let "z" (Summand (Term (Factor (Int 4))))
    (Summand (Plus
      (Minus
        (Term (Times (Factor (Var "x")) (Int 3)))
        (Factor (Brack (Summand (Minus (Term (Factor (Int 1))) (Factor (Var "y"))))))
      )
      (Factor (Var "z"))
    ))

exampleSum :: Fun
exampleSum =
    def sum(n):
    if n == 1:
        return 1
    else:
        Fun("def" Identifier("sum_2") (Factor (Var "x") Factor(Var "y")):
            Return("return" (Summand (Plus (Term (Factor (Var "x") Factor (Var "y")))))
            "end")
        return sum_2(Factor((Var "n")), FunCall(sum Parameters ((Summand (Plus (Term (Factor(Var "n") Factor(Var "1"))))))))
    end


-- # free vars in the body of f: z
-- def f(x, y):
--   return z + x + y     # z, x, y

freeVars :: Exp -> [String]
freeVars exp =
  case exp of
    Let x e1 e2 -> freeVars e1 ++ (freeVars e2 \\ [x])
    Summand Summand   -> freeVarsSummand Summand

freeVarsSummand :: Summand -> [String]
freeVarsSummand Summand =
  case Summand of
    Plus e t  -> freeVarsSummand e ++ freeVarsTerm t
    Minus e t -> freeVarsSummand e ++ freeVarsTerm t
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
