module ToypyData
    where
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
    = Times Term BoolExp       -- t * be
    | Div Term BoolExp         -- t / be
    | BoolExp BoolExp          -- be
    deriving (Show)

data BoolExp
    = Equals BoolExp Factor          -- e1 == e2
    | NotEquals BoolExp Factor       -- e1 != e2
    -- | Less Exp Term            -- e1 <  e2
    -- | Greater Exp Term         -- e1 >  e2
    -- | Lequals Exp Term         -- e1 <= e2
    -- | Gequals Exp Term         -- e1 >= e2
    | Factor Factor 
    deriving (Show)


data Factor
    = Int Int
    | Var Identifier
    | Brack Exp
    | RoutineCall Identifier [Exp]
    deriving (Show)



freeVarsDef :: Def -> [String]
freeVarsDef (Def id parameters body) = 
    (freeVarsBody body \\ parameters) \\ [id]

freeVarsBody :: [Declaration] -> [String]
freeVarsBody decs = concatMap freeVarsDeclaration decs

freeVarsDeclaration :: Declaration -> [String]
freeVarsDeclaration d = 
    case d of
        Return exp -> freeVarsExp exp
        Statement st -> freeVarsStatement st

freeVarsStatement :: Statement -> [String]
freeVarsStatement st =
    case st of
        Assignment id exp -> freeVarsExp exp \\ [id]
        RoutineCallS id exps -> concatMap freeVarsExp exps
        WhileLoop exp body -> freeVarsExp exp ++ freeVarsBody body
        ForLoop id exp1 exp2 body -> ((freeVarsExp exp1) ++ (freeVarsExp exp2) ++ (freeVarsBody body)) \\ [id]
        IfStatement exp ifbody (Just elsebody) -> freeVarsExp exp ++ freeVarsBody ifbody ++ freeVarsBody elsebody
        IfStatement exp ifbody Nothing -> freeVarsExp exp ++ freeVarsBody ifbody
        Print exps -> concatMap freeVarsExp exps
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
    Times t be -> freeVarsTerm t ++ freeVarsBoolExp be
    Div t be   -> freeVarsTerm t ++ freeVarsBoolExp be
    BoolExp be  -> freeVarsBoolExp be

freeVarsBoolExp :: BoolExp -> [String]
freeVarsBoolExp bexp = 
    case bexp of
        Equals be f    -> freeVarsBoolExp be ++ freeVarsFactor f
        NotEquals be f -> freeVarsBoolExp be ++ freeVarsFactor f
        Factor f       -> freeVarsFactor f


freeVarsFactor :: Factor -> [String]
freeVarsFactor f =
  case f of
    Var x               -> [x]
    Int _               -> []
    Brack e             -> freeVarsExp e
    RoutineCall id exps -> id : concatMap freeVarsExp exps