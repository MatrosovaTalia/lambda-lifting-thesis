module Main where

import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)
import           Data.List       ((\\))
import           Text.Show
import           System.IO
import           Control.Monad



main :: IO ()
main = do 
  handle <- openFile "sum_after.py" ReadMode 
  input <- hGetContents handle
  let tokens = resolveLayout True (myLexer input)
  case pProgram tokens of
    Left err -> print err
    Right program -> do
      putStrLn "Before:"
      putStrLn (printTree program)
      putStrLn "=============================="
      putStrLn "After:"
      putStrLn (printTree (freeVars program))


freeVars :: Program -> [Ident]
freeVars (Program decls) = concatMap freeVarsDecl  decls

freeVarsDecl :: Decl -> [Ident]
freeVarsDecl (DeclReturn exp)         = freeVarsExpr exp 
freeVarsDecl (DeclStatement st)       = freeVarsStatement st
freeVarsDecl (DeclDef id params body) = (concatMap freeVarsDecl body \\ params) \\ [id]

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
