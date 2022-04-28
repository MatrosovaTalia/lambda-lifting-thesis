module Main where

import           Program.Abs
import           Program.Layout (resolveLayout)
import           Program.Par    (myLexer, pProgram)
import           Program.Print  (printTree, Print)
import           Data.List      ((\\), isPrefixOf, elemIndices, partition, nub)
import           Data.Maybe     (listToMaybe)
import           Text.Show
import           System.IO
import           Control.Monad
import Text.XHtml (body)
import Foreign (free)
import Control.Monad.State (State)
-- import           Data.Map


-- stage1: generate recursive AST
generateRecursiveAST :: Program -> AST
treeToVectorTree :: AST -> VectorTree
treeToVectorTree tree = (depthAcc, typesAcc, valuesAcc)
    where
        treeToList currLevel (Tree root children)
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (treeToList (currLevel + 1)) children

        listTree = treeToList 0 tree

        depthVector = P.map (\(d, _, _) -> d) listTree
        types  = padToEqualLength '\0' $ P.map (\(_, t, _) -> t) listTree
        values = padToEqualLength '\0' $ P.map (\(_, _, v) -> v) listTree

        maxLength arr = P.maximum (P.map P.length arr)

        depthAcc = fromList (Z :. P.length depthVector) depthVector
        typesAcc = fromList (Z :. P.length types :. maxLength types) (P.concat types)
        valuesAcc = fromList (Z :. P.length values :. maxLength values) (P.concat values)



-- stage2: calculate scopes number -> list on Nodes (scopes, de Bruijn ids, depth, value , type)
getScopesCount :: AST -> [(Ident, Int)]

-- -- stage3: depth scopes
-- getDeBruijn :: AST -> [(Ident, Int)]

--stage4: Construct PAST
buildParrallelAST :: AST -> PAST

--stage5: lift
liftPast :: PAST -> PAST

--stage6: convert PAST to AST
convertToAST :: PAST -> AST

printResult :: AST -> Text