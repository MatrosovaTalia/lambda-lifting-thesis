module ParallelLift where

-- import           ScopesParallel



-- -- data PAST = PAST
-- --   { depthVector    :: Acc (Vector Int)
-- --   , levelVector    :: Acc (Vector Int)
-- --   , nodeTypeVector :: Acc (Vector NodeType)
-- --   , nodeCoords     :: Acc NodeCoords
-- --   } deriving (Show)

-- data NodeEntry = NodeEntry
--   { depth      :: Int
--   , level      :: Int
--   , nodeCoords :: [Int]
--   , nodeType   :: NodeType  
--   } deriving (Show, Eq)

-- parallelLift :: PAST -> PAST
-- parallelLift past = filterBoundVars past


-- -- if s_a - d_a >= s_f then lift is needed.
-- -- s_a = scope alpha
-- -- d_a = depth alpha (distance between alpha definition and alpha call)
-- -- s_f = scope f
-- filterBoundVars :: PAST -> PAST
-- filterBoundVars dv lv ntv nc = 
--     -- for each a:
--         -- if deBruijn:
--             -- from DeBruijn nested index (ni) and scope index (si) find f in past
--             -- if si_a - ni_a >= si_f : then rebind vars

--     if scope_def_f >= scope_f -> updatePAST
--         where 
--             updatePAST = addParamsDef + addParamsCall
    

--     findOuterDef dv lv ntv nc = \case
--         lv - dv >= dv_outer -> rebindVars inner (dv lv ntv nc) inner outer




-- findOuterDef :: PAST -> NodeEntry --need to check type. should be an exact node in node coods?
-- findOuterDef = scan PAST from current row up by scope index 

-- rebindVars :: PAST -> NodeEntry -> NodeEntry -> PAST
-- rebindVars past inner outher = updatePAST innerDepth'
--     where
--         inner = NodeEntry innerDepth innerLevel _nc innerType
--         outer = NodeEntry outerDepth outerLevel _nc outerType
--         innerDepth' = innerLevel - outerLevel

-- updatePAST :: PAST -> NodeEntry-> PAST
-- updatePAST -> somehow.


-- addParamsCall :: NodeType -> NodeType
-- addParamsCall

-- addParamsDef :: NodeType -> NodeType


