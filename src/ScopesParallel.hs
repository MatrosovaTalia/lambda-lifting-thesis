{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module ScopesParallel where

import           Ast.Abs
import           Data.Array.Accelerate             (Acc, Elt, Matrix, Vector,
                                                    Z (..), boolToInt, constant,
                                                    generate, pattern I1,
                                                    pattern I2, (!), (:.) (..))
import qualified Data.Array.Accelerate             as Acc
import qualified Data.Array.Accelerate.LLVM.Native as Acc
import           Data.List                         (elemIndex, zip4)
import           GHC.Generics                      (Generic)
import           Recursive                         (FreeAndDeclared (..),
                                                    freeAndDeclaredDecl)

data NodeType
    = NodeProgram
    -- Declaration nodes:
    | NodeDeclDef Int -- arity of function
    -- Statement nodes:
    | NodeAssign DeBruijnIndex
    | NodeWhileLoop
    | NodeForLoop
    | NodeIf
    | NodeRoutineCall DeBruijnIndex
    -- Expression nodes:
    | NodeInt Int
    | NodeVar DeBruijnIndex
    | NodeRCall DeBruijnIndex
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

data DeBruijnIndex = DeBruijnIndex
  { nestedIndex :: Int
  , scopeIndex  :: Int
  } deriving (Show, Eq, Generic, Elt)

data Entry = Entry {
    entryDepth    :: Int,
    entryLevel    :: Int,
    entryNodeType :: NodeType
  } deriving (Show, Eq)

type NodeCoords = Matrix Int

data PAST = PAST
  { depthVector    :: Acc (Vector Int)
  , levelVector    :: Acc (Vector Int)
  , nodeTypeVector :: Acc (Vector NodeType)
  , nodeCoords     :: Acc NodeCoords
  } deriving (Show)

toPAST :: Ast -> PAST
toPAST (Ast decls) = PAST {..}
  where
    entries = blockToEntry emptyContext decls
    depthList = map entryDepth entries
    levelList = map entryLevel entries
    nodeTypeList = map entryNodeType entries
    n = length depthList
    depthVector = Acc.use (Acc.fromList (Z:.n) depthList)
    levelVector = Acc.use (Acc.fromList (Z:.n) levelList)
    nodeTypeVector = Acc.use (Acc.fromList (Z:.n) nodeTypeList)
    nodeCoords = depthsToNodeCoords (maximum depthList) n depthVector


-- | Pretty-print a 'PAST'.
ppPAST :: PAST -> String
ppPAST PAST{..} = unlines $ map f (zip4 ds lvls coords ns)
  where
    f (d, l, c, n) = d <> " \t " <> l <> " \t " <> c <> " \t " <> n
    ds     = show <$> Acc.toList (Acc.run depthVector)
    lvls   = show <$> Acc.toList (Acc.run levelVector)
    coords = drop 1 (lines (show (Acc.run nodeCoords)))
    ns     = show <$> Acc.toList (Acc.run nodeTypeVector)

-- | Compute node coordinates from depth vector.
depthsToNodeCoords
  :: Int              -- ^ Maximum allowed depth.
  -> Int              -- ^ Number of elements (length of the depth vector).
  -> Acc (Vector Int) -- ^ Depth vector (prepared on GPU).
  -> Acc NodeCoords   -- ^ Node coordinates (prepared on GPU).
depthsToNodeCoords maxDepth n depthVector = do
  let dims = I2 (constant maxDepth) (constant n)
      depthMatrix = generate dims $ \(I2 i j) ->
        boolToInt ((depthVector ! (I1 j)) Acc.== i)
      scanMatrix = Acc.transpose (Acc.scanl1 (+) depthMatrix)
  Acc.imap (\(I2 i j) x -> x * boolToInt (j Acc.<= depthVector ! (I1 i))) scanMatrix

type Scope = [Ident]

data Context = Context {
    contextDepth  :: Int,
    contextLevel  :: Int,
    contextScopes :: [Scope]
}

emptyContext :: Context
emptyContext = Context
  { contextDepth = 0
  , contextLevel = 0
  , contextScopes = []
  }

-- | Convert a block of 'Decl's into a list of 'Entry's for parallel representation.
--
-- >>> decls = [DeclStatement (Assign (Ident "x") (EInt 3)),DeclStatement (Assign (Ident "y") (EPlus (EVar (Ident "x")) (ETimes (EInt 4) (ERCall (Ident "x") [EInt 3]))))]
-- >>> putStr $ ppNodesWithDepth $ (\e -> (entryDepth e, entryNodeType e)) <$> blockToEntry ScopesParallel.emptyContext decls
-- NodeAssign (DeBruijnIndex {nestedIndex = 0, scopeIndex = 0})
-- └─ NodeInt 3
-- NodeAssign (DeBruijnIndex {nestedIndex = 0, scopeIndex = 1})
-- └─ NodePlus
--    ├─ NodeVar (DeBruijnIndex {nestedIndex = 0, scopeIndex = 0})
--    └─ NodeTimes
--       ├─ NodeInt 4
--       └─ NodeRCall (DeBruijnIndex {nestedIndex = 0, scopeIndex = 0})
--          └─ NodeInt 3
blockToEntry :: Context -> [Decl] -> [Entry]
blockToEntry context@Context{..} decls = concatMap (declToEntry newContext) decls
  where
    FreeAndDeclared _fs localVars = foldMap freeAndDeclaredDecl decls
    newContext = context
        { contextScopes = map fst localVars : contextScopes }

declToEntry :: Context -> Decl -> [Entry]
declToEntry context@Context{..} = \case
  DeclStatement st -> stToEntry context st
  DeclReturn    expr -> exprToEntry context expr
  DeclDef (RoutineDecl _f params body) -> -- NOTE: _f was added to context in blockToEntry!
    let newContext = context
          { contextDepth = contextDepth + 1
          , contextLevel = contextLevel + 1
          , contextScopes = params : contextScopes
          }
     in mkNode (NodeDeclDef (length params)) : blockToEntry newContext body
  where
    mkNode ty = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryNodeType = ty
    }

stToEntry :: Context -> Statement -> [Entry]
stToEntry context@Context{..} = \case
  Assign x expr -> mkNode (NodeAssign (findVarInScopes' x contextScopes)) : goExpr expr
  WhileLoop expr decls     -> mkNode NodeWhileLoop : goExpr expr ++ goBlock decls
  ForLoop x expr1 expr2 decls ->
    let newContext = context
          { contextDepth = contextDepth + 1
          , contextLevel = contextLevel + 1
          , contextScopes = [x] : contextScopes
          }
     in mkNode NodeForLoop : goExpr expr1 ++ goExpr expr2 ++ blockToEntry newContext decls
  If expr decls            -> mkNode NodeIf : goExpr expr ++ goBlock decls
  IfElse expr declsThen declsElse -> mkNode NodeIf : goExpr expr ++ goBlock declsThen ++ goBlock declsElse
  RoutineCall f exprs     -> mkNode (NodeRoutineCall (findVarInScopes' f contextScopes)) : concatMap goExpr exprs

  where
    goExpr = exprToEntry context { contextDepth = contextDepth + 1 }
    goBlock = blockToEntry context { contextDepth = contextDepth + 1 }

    mkNode ty = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryNodeType = ty
    }


exprToEntry :: Context -> Expr -> [Entry]
exprToEntry context@Context{..} = \case
  EInt n        -> [mkNode (NodeInt (fromInteger n))]
  EVar x        -> [mkNode (NodeVar (findVarInScopes' x contextScopes))]
  ETimes  e1 e2 -> mkNode NodeTimes   : go e1 ++ go e2
  EDiv    e1 e2 -> mkNode NodeDiv     : go e1 ++ go e2
  ERem    e1 e2 -> mkNode NodeRem     : go e1 ++ go e2
  EPlus   e1 e2 -> mkNode NodePlus    : go e1 ++ go e2
  EMinus  e1 e2 -> mkNode NodeMinus   : go e1 ++ go e2
  EAND    e1 e2 -> mkNode NodeAND     : go e1 ++ go e2
  EOR     e1 e2 -> mkNode NodeOR      : go e1 ++ go e2
  EXOR    e1 e2 -> mkNode NodeXOR     : go e1 ++ go e2
  ELess   e1 e2 -> mkNode NodeLess    : go e1 ++ go e2
  EGrt    e1 e2 -> mkNode NodeGrt     : go e1 ++ go e2
  EELess  e1 e2 -> mkNode NodeELess   : go e1 ++ go e2
  EEGrt   e1 e2 -> mkNode NodeEGrt    : go e1 ++ go e2
  EEQUAL  e1 e2 -> mkNode NodeEQUAL   : go e1 ++ go e2
  ENEQUAL e1 e2 -> mkNode NodeNEQUAL  : go e1 ++ go e2
  ERCall  f  es -> mkNode (NodeRCall(findVarInScopes' f contextScopes) )   : concatMap go es
  ENeg    e     -> mkNode NodeNeg     : go e
  ENot    e     -> mkNode NodeNot     : go e
  where
    go = exprToEntry context { contextDepth = contextDepth + 1 }

    mkNode ty = Entry {
        entryDepth = contextDepth,
        entryLevel = contextLevel,
        entryNodeType = ty
    }

findVarInScopes :: Ident -> [Scope] -> Maybe DeBruijnIndex
findVarInScopes _ [] = Nothing
findVarInScopes x (scope:scopes) =
    case elemIndex x scope of
        Nothing ->
            case findVarInScopes x scopes of
                Nothing                  -> Nothing
                Just (DeBruijnIndex s i) -> Just (DeBruijnIndex (s + 1) i)
        Just i -> Just (DeBruijnIndex 0 i)

findVarInScopes' :: Ident -> [Scope] -> DeBruijnIndex
findVarInScopes' x scopes =
  case findVarInScopes x scopes of
    Nothing -> error ("cannot find variable " <> show x <> " in scopes " <> show scopes)
    Just i -> i

-- | Pretty-print depth-annotated nodes as a ASCII tree-like structure.
--
-- >>> putStr $ ppNodesWithDepth (zip [0,1,2,1,2,3,3,2,3] [0..])
-- 0
-- ├─ 1
-- │  └─ 2
-- └─ 3
--    ├─ 4
--    │  ├─ 5
--    │  └─ 6
--    └─ 7
--       └─ 8
ppNodesWithDepth :: Show a => [(Int, a)] -> String
ppNodesWithDepth xs = unlines $
  zipWith (\t x -> drop 3 $ t <> " " <> show (snd x)) (depthsTree (map fst xs)) xs

-- | Pretty-print an ASCII tree-like structure based on depth list.
--
-- >>> mapM_ putStrLn $ depthsTree [0,1,2,1,2,3,3,2,3]
-- └─
--    ├─
--    │  └─
--    └─
--       ├─
--       │  ├─
--       │  └─
--       └─
--          └─
depthsTree :: [Int] -> [String]
depthsTree = map (<> "─") . scanr1 prev . map defaultLine
  where
    defaultLine i = replicate (3 * i) ' ' <> "└"

    prev i []   = i
    prev i next = zipWith g i (next ++ repeat ' ')
      where
        g ' ' '│' = '│'
        g ' ' '└' = '│'
        g ' ' '├' = '│'
        g '─' '└' = '┬'
        g '└' '│' = '├'
        g '└' '└' = '├'
        g '─' '├' = '┬'
        g c _     = c

