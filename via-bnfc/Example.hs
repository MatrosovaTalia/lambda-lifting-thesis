module Main where

import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)

main :: IO ()
main = do
  input <- getContents
  let tokens = resolveLayout True (myLexer input)
  case pProgram tokens of
    Left err -> print err
    Right program -> do
      putStrLn "Before:"
      putStrLn (printTree program)
      putStrLn "=============================="
      putStrLn "After:"
      putStrLn (printTree (transform program))

transform :: Program -> Program
transform (Program decls) = Program (map transformDecl decls)

transformDecl :: Decl -> Decl
transformDecl (DeclReturn expr) = DeclReturn (transformExpr expr)
transformDecl (DeclStatement s) = DeclStatement (transformStatement s)
transformDecl (DeclDef (RoutineDecl f args decls)) = DeclDef
  (RoutineDecl f (reverse args) (map transformDecl decls))

transformStatement :: Statement -> Statement
transformStatement (Assign x expr) = Assign x (transformExpr expr)
transformStatement (If cond tru)
  = If (transformExpr cond) (map transformDecl tru)
transformStatement (IfElse cond tru fls)
  = IfElse (transformExpr cond) (map transformDecl tru) (map transformDecl fls)
transformStatement (WhileLoop cond decls) = WhileLoop
  (transformExpr cond) (map transformDecl decls)
transformStatement (ForLoop i from to decls) = ForLoop
  i (transformExpr from) (transformExpr to) (map transformDecl decls)
transformStatement (RoutineCall f args) = RoutineCall f (reverse args)


transformExpr :: Expr -> Expr
transformExpr expr =
  case expr of
    EInt n     -> expr
    EVar x     -> expr
    ENot e     -> ENot (transformExpr e)
    EPlus l r  -> EPlus (transformExpr l) (transformExpr r)
    EMinus l r -> EMinus (transformExpr l) (transformExpr r)
    ETimes l r -> ETimes (transformExpr l) (transformExpr r)
    EDiv l r   -> EDiv (transformExpr l) (transformExpr r)
    ERem l r   -> ERem (transformExpr l) (transformExpr r)
    EAND l r   -> EAND (transformExpr l) (transformExpr r)
    EOR l r    -> EOR (transformExpr l) (transformExpr r)
    EXOR l r   -> EXOR (transformExpr l) (transformExpr r)

