{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

import GHC.Generics (Generic)
import Data.Array.Accelerate (Elt)
import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)
import           Data.List      ((\\), isPrefixOf, elemIndices, partition, nub)

data NodeType
    = NodeProgram
    -- Declaration nodes:
    | NodeDeclDef Int -- arity of function
    -- Expression nodes:
    | NodeInt Int
    | NodeVar
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


-- Добавить в текст:
-- Node type с Node value, а не как у Аарона: у него отдельный массив.
-- Блоки и scopes в рекурсивной и параллельной реализации: сперва все переменные ищутся во внешнем блоке,
-- добавляются в текущий скоуп, а потом из блока вызываются дальнейшие функции.

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
  ...
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

exprToEntry :: Context -> Expr -> [Entry]
exprToEntry context@Context{..} = \case
  EInt n -> [mkNode (NodeInt n)]
  EVar x ->
      case findVarInScopes x contextScopes of
          Nothing       -> error "undefined variable"
          Just (s, i)   -> Just (s, i)
  ETimes e1 e2 -> mkNode NodeTimes : (go e1 ++ go e2)
  ...
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