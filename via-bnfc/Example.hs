module Main where

import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)
import           Data.List       ((\\))
import           Text.Show
import           System.IO
import           Control.Monad
import           Data.Map



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
      putStrLn "After:"
      putStrLn (printTree (getFreeVars program))


getFreeVars :: Program -> [(Ident, [Ident])]
getFreeVars (Program decls) = concatMap getFreeVarsDecl  decls

freeVarsDef :: Decl -> [(Ident, [Ident])]
freeVarsDef (DeclDef id params body) = [(id, concatMap  (t a)]

getFreeVarsDecl :: Decl -> [Ident]
getFreeVarsDecl (DeclReturn exp)         = getFreeVarsExpr exp 
getFreeVarsDecl (DeclStatement st)       = getFreeVarsStatement st
getFreeVarsDecl (DeclDef id params body) = (concatMap getFreeVarsDecl body \\ params) \\ [id]

getFreeVarsStatement :: Statement -> [Ident]
getFreeVarsStatement (Assign x expr) = getFreeVarsExpr expr \\ [x]
getFreeVarsStatement (If cond tru) = 
                    getFreeVarsExpr cond ++ concatMap getFreeVarsDecl tru
getFreeVarsStatement (IfElse cond tru fls) = 
                    getFreeVarsExpr cond ++ concatMap getFreeVarsDecl tru ++ concatMap getFreeVarsDecl fls
getFreeVarsStatement (WhileLoop cond decls) = 
                    getFreeVarsExpr cond ++ concatMap getFreeVarsDecl decls
getFreeVarsStatement (ForLoop i from to decls) = 
                    (getFreeVarsExpr from ++ getFreeVarsExpr to ++ concatMap getFreeVarsDecl decls) \\ [i]
getFreeVarsStatement (RoutineCall f args) = f : concatMap getFreeVarsExpr args



getFreeVarsExpr :: Expr -> [Ident]
getFreeVarsExpr expr =
  case expr of
    EInt _          -> []
    EVar x          -> [x]
    ERCall id exprs -> id : concatMap getFreeVarsExpr exprs 
    ENot expr       -> getFreeVarsExpr expr
    ENeg expr       -> getFreeVarsExpr expr
    EPlus l r       -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EMinus l r      -> getFreeVarsExpr l ++ getFreeVarsExpr r
    ETimes l r      -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EDiv l r        -> getFreeVarsExpr l ++ getFreeVarsExpr r
    ERem l r        -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EAND l r        -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EOR l r         -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EXOR l r        -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EEQUAL l r      -> getFreeVarsExpr l ++ getFreeVarsExpr r
    ENEQUAL l r     -> getFreeVarsExpr l ++ getFreeVarsExpr r
    ELess l r       -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EGrt l r        -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EELess l r      -> getFreeVarsExpr l ++ getFreeVarsExpr r
    EEGrt l r       -> getFreeVarsExpr l ++ getFreeVarsExpr r


-- liftFreeVars :: []