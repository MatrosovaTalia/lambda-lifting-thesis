{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

import GHC.Generics (Generic)
import Data.Array.Accelerate (Elt)
import           Ast.Abs
import           Ast.Layout (resolveLayout)
import           Ast.Par    (myLexer, pProgram)
import           Ast.Print  (printTree, Print)
import           Data.List      ((\\), isPrefixOf, elemIndices, partition, nub)

data NodeType
    = NodeProgram
    -- Declaration nodes:
    | NodeDeclDef Int -- arity of function
    -- Expression nodes:
    | NodeInt Int    | NodeVar
    | NodeRCall
    | NodeNeg
    | NodeNot
    | NodeTimes
    | NodeDiv
    | NodeRem
    | NodePlus
    | NodeMinus
    | NodeAND
    | NodeOR
    | NodeXOR
    | NodeLess
    | NodeGrt
    | NodeELess
    | NodeEGrt
    | NodeEQUAL
    | NodeNEQUAL
    deriving (Show, Eq, Generic, Elt)


data Entry = Entry {
    entryDepth          :: Int,
    entryLevel          :: Int,
    entryDeBruijnIndex  :: Maybe (Int, Int),
    entryNodeType       :: NodeType
  } deriving (Show, Eq)

type Scope = [Ident]

data Context = Context {
    contextDepth :: Int,
    contextLevel :: Int,
    contextScopes :: [Scope]
}

blockToEntry :: Context -> [Decl] -> [Entry]
blockToEntry context@Context{..} decls = concatMap (declToEntry newContext) decls
  where
    localVars = contatMap ... decls
    newContext = context
        { contextScopes = localVars : contextScopes }

declToEntry :: Context -> Decl -> [Entry]
declToEntry context@Context{..} = \case
  DeclStatement st -> stToEntry st
  DeclReturn    expr -> exprToEntry expr
  DeclDef (RoutineDecl f params body) ->
    let newContext = context
          { contextDepth = contextDepth + 1
          , contextLevel = contextLevel + 1
          , contextScopes = params : contextScopes
          }
     in mkNode (NodeDeclDef (length params)) : concatMap (declToEntry newContext) body
  where
    mkNode ty value = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryDeBruijnIndex = Nothing,
        entryNodeType = ty
    }
  
stToEntry :: Context -> Statement -> [Entry]
stToEntry context@Context{..} = \case
  Assign id expr            -> mkNode (NodeAssign (findVarInScopes id contextScopes)) : goExpr expr : [] --new var old scope
  WhileLoop expr decls     -> mkNode NodeWhileLoop : goExpr expr : map goExpr decls
  ForLoop id expr1 expr2 decls ->
    let newContext = context
          { contextDepth = contextDepth + 1
          , contextLevel = contextLevel + 1
          , contextScopes = [id] : contextScopes
          }
     in mkNode NodeForLoop : goExpr expr1 : goExpr expr2 : map (stToEntry newContext) decls
  If expr decls            -> mkNode NodeIf : goExpr expr : map goExpr decls
  IfElse expr declsThen declsElse -> mkNode NodeIf : goExpr expr : map goExpr (declsThen ++ declsElse)
  RoutineCall id exprs     -> mkNode (NodeRoutineCall (findVarInScopes id contextScopes)) : map goExpr exprs

  where
    goExpr = exprToEntry context { contextDepth = contextDepth + 1 }

    mkNode ty value = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryDeBruijnIndex = Nothing,
        entryNodeType = ty
    }
  

exprToEntry :: Context -> Expr -> [Entry]
exprToEntry context@Context{..} = \case
  EInt n -> [mkNode (NodeInt n)]
  EVar x ->
      case findVarInScopes x contextScopes of
          Nothing       -> error "undefined variable"
          Just (s, i)   -> Just (s, i)
  ETimes  e1 e2  -> mkNode NodeTimes : (go e1 ++ go e2)
  EDiv    e1 e2  -> mkNode NodeDiv : (go e1 ++ go e2)
  ERem    e1 e2  -> mkNode NodeRem : (go e1 ++ go e2)
  EPlus   e1 e2  -> mkNode NodePlus : (go e1 ++ go e2)
  EMinus  e1 e2  -> mkNode NodeMinus : (go e1 ++ go e2)
  EAND    e1 e2  -> mkNode NodeAnd : (go e1 ++ go e2)
  EOR     e1 e2  -> mkNode NodeOr : (go e1 ++ go e2)
  EXOR    e1 e2  -> mkNode NodeXor : (go e1 ++ go e2)
  ELess   e1 e2  -> mkNode NodeLess : (go e1 ++ go e2)
  EGrt    e1 e2  -> mkNode NodeGrt : (go e1 ++ go e2)
  EELess  e1 e2  -> mkNode NodeELess : (go e1 ++ go e2)
  EEGrt   e1 e2  -> mkNode NodeEGrt : (go e1 ++ go e2)
  EEQUAL  e1 e2  -> mkNode NodeEQUAL : (go e1 ++ go e2)
  ENEQUAL e1 e2  -> mkNode NodeNEQUAL : (go e1 ++ go e2)
  ERCall  id [e] -> mkNode NodeRCall : (go e1 ++ go e2)
  ENeg    e      -> mkNode NodeNeg : (go e)
  ENot    e      -> mkNode NodeNot        : (go e)
  where
    go = exprToEntry context { contextDepth = contextDepth + 1 }

    mkNode ty value = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryDeBruijnIndex = Nothing,
        entryNodeType = ty
    }

findVarInScopes :: Ident -> [Scope] -> Maybe (Int, Int)
findVarInScopes _ [] = Nothing
findVarInScopes x (scope:scopes) =
    case elemIndex x scope of
        Nothing ->
            case findVarInScopes x scopes of
                Nothing -> Nothing
                Just (s, i) -> Just (s + 1, i)
        Just i -> Just (0, i)





