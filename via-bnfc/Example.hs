module Main where

import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)
import           Data.List      ((\\), isPrefixOf, elemIndices)
-- import           Data.String
import           Data.Maybe     (listToMaybe)
import           Text.Show
import           System.IO
import           Control.Monad
import Text.XHtml (body)
import Foreign (free)
import Control.Monad.State (State)
-- import           Data.Map



main :: IO ()
main = do 
  handle <- openFile "sum.py" ReadMode 
  input <- hGetContents handle
  let tokens = resolveLayout True (myLexer input)
  case pProgram tokens of
    Left err -> print err
    Right program -> do
      putStrLn "Before:"
      putStrLn (printTree program)
      putStrLn "=============================="
      putStrLn "freeVars:"
      putStrLn (printTree (freeVars program))
      putStrLn "freeAndDeclared:"
      print (freeAndDeclared program)
      putStrLn "Update Routine Decl:"
      putStrLn (printTree (addParams program))

data FreeAndDeclared = FreeAndDeclared
  { freeIdents     :: [Ident]
  , declaredIdents :: [(Ident, [Ident])]
  } deriving (Show)

instance Semigroup FreeAndDeclared where
  FreeAndDeclared fs1 ds1 <> FreeAndDeclared fs2 ds2 =
    FreeAndDeclared (fs1 <> fs2) (ds1 <> ds2)

instance Monoid FreeAndDeclared where
  mempty = FreeAndDeclared [] [] 

freeAndDeclared :: Program -> FreeAndDeclared
freeAndDeclared (Program decls) = foldMap freeAndDeclaredDecl decls

freeAndDeclaredDecl :: Decl -> FreeAndDeclared
freeAndDeclaredDecl (DeclReturn exp) = freeAndDeclaredExpr exp
freeAndDeclaredDecl (DeclStatement st) = freeAndDeclaredStatement st
freeAndDeclaredDecl (DeclDef def) = freeAndDeclaredDef def

freeAndDeclaredBlock :: [Decl] -> FreeAndDeclared
freeAndDeclaredBlock decls = FreeAndDeclared (fs \\ map fst ds) []
  where
    FreeAndDeclared fs ds = foldMap freeAndDeclaredDecl decls

freeAndDeclaredExpr :: Expr -> FreeAndDeclared
freeAndDeclaredExpr expr = FreeAndDeclared (freeVarsExpr expr) []

without :: FreeAndDeclared -> [Ident] -> FreeAndDeclared
FreeAndDeclared fs ds `without` locals = FreeAndDeclared (fs \\ locals) (filter (\(f, _xs) -> f `notElem` locals) ds)

freeAndDeclaredStatement :: Statement -> FreeAndDeclared
freeAndDeclaredStatement st =
  case st of
    Assign x expr -> freeAndDeclaredExpr expr <> FreeAndDeclared [] [(x, [])]
    If cond tru -> freeAndDeclaredExpr cond <> freeAndDeclaredBlock tru
    IfElse cond tru fls -> freeAndDeclaredExpr cond <> freeAndDeclaredBlock tru <> freeAndDeclaredBlock fls
    WhileLoop cond decls ->
                    freeAndDeclaredExpr cond <> freeAndDeclaredBlock decls
    ForLoop i from to decls -> foldMap freeAndDeclaredExpr [from, to] <> (freeAndDeclaredBlock decls `without` [i])
    RoutineCall f args -> FreeAndDeclared [f] [] <> foldMap freeAndDeclaredExpr args

freeAndDeclaredDef :: RoutineDecl -> FreeAndDeclared
freeAndDeclaredDef (RoutineDecl f params body) = FreeAndDeclared xs [(f, xs)]
  where
    xs = fs \\ (f : params)
    FreeAndDeclared fs _ds = freeAndDeclaredBlock body

-- Stage1
freeVars :: Program -> [[Ident]]
freeVars (Program decls) = map freeVarsDecl decls


freeVarsDef :: RoutineDecl  -> [Ident]
freeVarsDef (RoutineDecl id params body) = bodyFreeVars \\ params
  where 
    bodyFreeVars = concatMap freeVarsDecl body


freeVarsDecl :: Decl  -> [Ident]
freeVarsDecl (DeclReturn exp)   = freeVarsExpr exp
freeVarsDecl (DeclStatement st) = freeVarsStatement st
freeVarsDecl (DeclDef def)      = freeVarsDef def

freeVarsStatement :: Statement -> [Ident]
freeVarsStatement (Assign x expr) = freeVarsExpr expr \\ [x]
freeVarsStatement (If cond tru) = 
                    freeVarsExpr cond ++ concatMap freeVarsDecl tru
freeVarsStatement (IfElse cond tru fls) = 
                    freeVarsExpr cond ++ concatMap freeVarsDecl tru ++ concatMap freeVarsDecl fls
freeVarsStatement (WhileLoop cond decls) = 
                    freeVarsExpr cond ++ concatMap freeVarsDecl decls
freeVarsStatement (ForLoop i from to decls) = 
                    (freeVarsExpr from ++ freeVarsExpr to ++ concatMap freeVarsDecl decls) \\ [i]
freeVarsStatement (RoutineCall f args) = f : concatMap freeVarsExpr args



freeVarsExpr :: Expr -> [Ident]
freeVarsExpr expr =
  case expr of
    EInt _          -> []
    EVar x          -> [x]
    ERCall id exprs -> id : concatMap freeVarsExpr exprs 
    ENot expr       -> freeVarsExpr expr
    ENeg expr       -> freeVarsExpr expr
    EPlus l r       -> freeVarsExpr l ++ freeVarsExpr r
    EMinus l r      -> freeVarsExpr l ++ freeVarsExpr r
    ETimes l r      -> freeVarsExpr l ++ freeVarsExpr r
    EDiv l r        -> freeVarsExpr l ++ freeVarsExpr r
    ERem l r        -> freeVarsExpr l ++ freeVarsExpr r
    EAND l r        -> freeVarsExpr l ++ freeVarsExpr r
    EOR l r         -> freeVarsExpr l ++ freeVarsExpr r
    EXOR l r        -> freeVarsExpr l ++ freeVarsExpr r
    EEQUAL l r      -> freeVarsExpr l ++ freeVarsExpr r
    ENEQUAL l r     -> freeVarsExpr l ++ freeVarsExpr r
    ELess l r       -> freeVarsExpr l ++ freeVarsExpr r
    EGrt l r        -> freeVarsExpr l ++ freeVarsExpr r
    EELess l r      -> freeVarsExpr l ++ freeVarsExpr r
    EEGrt l r       -> freeVarsExpr l ++ freeVarsExpr r


-- Stage 2
addParams :: Program -> Program
addParams (Program decls) = Program (addParamsBlockWith [] decls)

addParamsBlockWith :: [(Ident, [Ident])] -> [Decl] -> [Decl]
addParamsBlockWith outerDefs decls = map (addParamsDeclWith (innerDefs <> outerDefs)) decls
  where
    FreeAndDeclared fs innerDefs = foldMap freeAndDeclaredDecl decls

addParamsDeclWith :: [(Ident, [Ident])] -> Decl -> Decl
addParamsDeclWith ds (DeclReturn exp) = DeclReturn (addParamsExprWith ds exp)
addParamsDeclWith ds (DeclStatement st) = DeclStatement (addParamsStatementWith ds st)
addParamsDeclWith ds (DeclDef def) = DeclDef (addParamsDefWith ds def)


-- Remove i in for loop & assign
addParamsStatementWith :: [(Ident, [Ident])] -> Statement -> Statement
addParamsStatementWith ds (Assign x expr) = Assign x (addParamsExprWith ds expr)
addParamsStatementWith ds (If cond tru) = If (addParamsExprWith ds cond) (addParamsBlockWith ds tru)
addParamsStatementWith ds (IfElse cond tru fls) = IfElse (addParamsExprWith ds cond) (addParamsBlockWith ds tru) (addParamsBlockWith ds fls)
addParamsStatementWith ds (WhileLoop cond decls) = WhileLoop (addParamsExprWith ds cond) (addParamsBlockWith ds decls)
addParamsStatementWith ds (ForLoop i from to decls) = ForLoop i (addParamsExprWith ds from) (addParamsExprWith ds to) (addParamsBlockWith ds decls)
addParamsStatementWith ds (RoutineCall f args) =
  case lookup f ds of
    Just moreArgs -> RoutineCall f (map (addParamsExprWith ds) args <> map EVar moreArgs)
    Nothing -> RoutineCall f (map (addParamsExprWith ds) args)

addParamsExprWith :: [(Ident, [Ident])] -> Expr -> Expr
addParamsExprWith ds expr =
  case expr of
    EInt{}        -> expr
    EVar{}        -> expr
    ERCall id exprs ->
      case lookup id ds of
        Just args -> ERCall id (map (addParamsExprWith ds) exprs ++ map EVar args)
        Nothing   -> ERCall id (map (addParamsExprWith ds) exprs)
    ENot expr       -> ENot (addParamsExprWith ds expr)
    ENeg expr       -> ENeg (addParamsExprWith ds expr)
    EPlus l r       -> EPlus (addParamsExprWith ds l) (addParamsExprWith ds r)
    EMinus l r      -> EMinus (addParamsExprWith ds l) (addParamsExprWith ds r)
    ETimes l r      -> ETimes (addParamsExprWith ds l) (addParamsExprWith ds r)
    EDiv l r        -> EDiv (addParamsExprWith ds l) (addParamsExprWith ds r)
    ERem l r        -> ERem (addParamsExprWith ds l) (addParamsExprWith ds r)
    EAND l r        -> EAND (addParamsExprWith ds l) (addParamsExprWith ds r)
    EOR l r         -> EOR (addParamsExprWith ds l) (addParamsExprWith ds r)
    EXOR l r        -> EXOR (addParamsExprWith ds l) (addParamsExprWith ds r)
    EEQUAL l r      -> EEQUAL (addParamsExprWith ds l) (addParamsExprWith ds r)
    ENEQUAL l r     -> ENEQUAL (addParamsExprWith ds l) (addParamsExprWith ds r)
    ELess l r       -> ELess (addParamsExprWith ds l) (addParamsExprWith ds r)
    EGrt l r        -> EGrt (addParamsExprWith ds l) (addParamsExprWith ds r)
    EELess l r      -> EELess (addParamsExprWith ds l) (addParamsExprWith ds r)
    EEGrt l r       -> EEGrt (addParamsExprWith ds l) (addParamsExprWith ds r)

addParamsDefWith :: [(Ident, [Ident])] -> RoutineDecl -> RoutineDecl 
addParamsDefWith ds def@(RoutineDecl id params body) = RoutineDecl id (params <> newParams) (addParamsBlockWith ds body)
  where
    newParams = fs \\ map fst ds
    FreeAndDeclared fs _ds = freeAndDeclaredDef def

addParamsDef :: RoutineDecl -> RoutineDecl 
addParamsDef def@(RoutineDecl id params body) = RoutineDecl id (params ++ newParams) (map addParamsDecl body)
  where
    FreeAndDeclared newParams _ds = freeAndDeclaredDef def

addParamsDecl :: Decl -> Decl 
addParamsDecl (DeclReturn ret)   = DeclReturn ret
addParamsDecl (DeclStatement st) = DeclStatement st
addParamsDecl (DeclDef def)      = DeclDef (addParamsDef def)


-- Stage 3 
-- addParamsCall ::  -> DeclStatement 
-- addParamsCall  Statement (RoutineCall id params) = Statement id params ++ freeVars



-- Найти все свободные переменные для каждой функции
-- -- (предполагаем, что все идентификаторы уникальные)
-- stage1 :: Program -> [Ident]
-- stage1 program = freeVars program

-- -- Для каждой фукнции, добавить свободные переменные в список формальных аргументов
-- stage2 :: Program -> Program
-- stage2 program = addParams 

-- -- Для каждого вызова функции, добавить свободные переменные в список аргументов
-- stage3 :: [(Ident, [Ident])] -> Program -> Program

-- -- Вытащить все локальные определения функций
-- stage3 :: Program -> [(Ident, [Ident], [Statement])]

-- -- Удалить все локальные определения функций
-- stage4 :: Program -> Program

-- -- Перевести все локальные определения в программу с глобально определёнными фукнциями
-- stage5 :: [(Ident, [Ident], [Statement])] -> Program

-- -- Конкатенация программ
-- stage6 :: Program -> Program -> Program
 --

-- ????
-- print [Ident] in freeVarsDecl (def)
-- define freeVars for Def and Decl return/ Statement separately.
