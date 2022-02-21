
import Data.List ((\\))
import Text.Show

type Program
    = [Statement]

type Identifier
    = String

type Body 
    = [Declaration]

data Declaration
    = Return Exp
    | Statement Statement
    deriving (Show)

data Statement 
    = Assignment Identifier Exp
    | RoutineCallS Identifier [Exp]
    | WhileLoop Exp Body
    | ForLoop Identifier Exp Exp Body
    | IfStatement Exp Body (Maybe Body)
    | Print [Exp]
    | RoutineDeclaration Def 
    deriving (Show)

data Def
    = Def Identifier Parameters Body
    deriving (Show)

type Parameters
    = [Identifier]

data Exp
    = Plus Exp Term          -- e + t
    | Minus Exp Term         -- e - t
    | Term Term               -- t
    deriving (Show)

data Term
    = Times Term Factor       -- t * f
    | Div Term Factor         -- t / f
    | Factor Factor           -- f
    deriving (Show)

data Factor
    = Int Int
    | Var Identifier
    | Brack Exp
    | RoutineCall Identifier [Exp]
    deriving (Show)



-- # free vars in the body of f: z
-- def f(x, y):
--   return z + x + y     # z, x, y

freeVarsDef :: Def -> [String]
freeVarsDef Def id parameters body = 
    (freeVarsBody body \\ parameters) \\ [id]

freeVarsBody :: Declaration -> [String]
freeVarsBody d = 
    case d of
        Return exp -> freeVarsExp exp
        Statement st -> freeVarsStatement st

freeVarsStatement :: Statement -> [String]
freeVarsStatement Statement =
    case Statement of
        Assignment id exp -> freeVarsExp exp \\ [id]
        RoutineCallS id [exp] -> concatMap freeVarsExp [exp]
        WhileLoop exp body -> freeVarsExp exp ++ freeVarsBody body
        ForLoop id exp1 exp2 body -> ((freeVarsExp exp1) ++ (freeVarsExp exp2) ++ (freeVarsBody body)) \\ [id]
        IfStatement exp ifbody (Just elsebody) -> freeVarsExp exp ++ freeVarsBody ifbody ++ freeVarsBody elsebody
        Print [exp] -> concatMap exp
        RoutineDeclaration def -> freeVarsDef def


freeVarsExp :: Exp -> [String]
freeVarsExp exp =
  case exp of
    Plus exp term  -> freeVarsExp exp ++ freeVarsTerm term
    Minus exp term -> freeVarsExp exp ++ freeVarsTerm term
    Term term    -> freeVarsTerm term

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
