module Language.Graphviz where

import Data.Expression
import Data.Expression.Const
import Data.List
import Data.List.Utils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Program
import System.Process
import Util

dotId :: [Label] -> Id -> String
dotId r j = case j of
  Dummy level i       -> prefix level ++"_i_"++ show level ++"_"++ show i
  Volatile ns level i -> prefix level ++"_x_"++ ns ++"_"++ show level ++"_"++ show i
  Internal level i    -> prefix level ++"_v_"++ show level ++"_"++ show i
  Symbol name _       -> name
  where prefix level = if level == 0 then "" else r !! (level - 1)

dotConst :: [Label] -> ConstVal -> String
dotConst r c = last r ++"_c_"++ (replace "." "_" $ replace "-" "_" $ show c)

dotConsts :: [Label] -> [NodeRef] -> String
dotConsts r js = unlines $ do
  j <- js
  case j of
    Var (Symbol s _) _ -> return $ last r ++ s  ++ prefix ++ s      ++"\"]"
    Const c _          -> return $ dotConst r c ++ prefix ++ show c ++"\"]"
    _ -> []
  where prefix = " [shape=\"plain\" label=\""

dotNodeRef :: [Label] -> Label -> NodeRef -> String
dotNodeRef r name (Index j js) =
  name ++" [label=\"!\"]\n"++
  dotConsts (r ++ [name]) (j:js)

dotNode :: [Label] -> Label -> Node -> String
dotNode _ _ (Apply "getExternal" _ _) = ""
dotNode r name (Apply "id" [j] _) = dotNodeRef r name j
dotNode r name (Apply f js _) =
  name ++" [label=\""++ f ++"\"]\n"++
  dotConsts (r ++ [name]) js
dotNode r name (Array sh (Lambda dag ret) _) =
  unlines [dotConsts (r' ++ [idx j]) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  "subgraph cluster_array_"++ name ++" {\n"++ indent (
    "label=\"array\"\n"++
    unlines [idx j ++" [shape=\"rectangle\" label=\"index "++ show i ++"\"]"
            | (i,j) <- [1..] `zip` inputs dag] ++"\n"++
    dotDAG r' dag (Just ret) ++"\n"++
    name ++ sret ++"style=\"bold\"]"
  ) ++"\n}"
  where r' = r ++ [name]
        sret = case ret of
          Var i _ -> " ["
          Const c _ -> " [label=\""++ show c ++"\" "
        idx j = if Just j == getId ret then name else dotId r' j
dotNode r name (FoldScan _ _ (Lambda dag ret) seed ls _) =
  dotConsts (r' ++ [elemt]) [ls] ++"\n"++
  dotConsts (r' ++ [accum]) [seed] ++"\n"++
  "subgraph cluster_foldscan_"++ name ++" {\n"++ indent (
    "label=\"fold/scan\"\n"++
    elemt ++" [shape=\"rectangle\" label=\"elem\"]\n"++
    accum ++" [shape=\"rectangle\" label=\"accum\"]\n"++
    dotDAG r' dag (Just ret) ++"\n"++
    name ++ sret ++"style=\"bold\"]"
  ) ++"\n}"
  where r' = r ++ [name]
        sret = case ret of
          Var i _ -> " ["
          Const c _ -> " [label=\""++ show c ++"\" "
        [i,j] = inputs dag
        elemt = if Just i == getId ret then name else dotId r' i
        accum = if Just j == getId ret then name else dotId r' j
dotNode _ _ n = error $ "dotNode "++ show n

dotDAG :: [Label] -> DAG -> Maybe NodeRef -> String
dotDAG r dag mr = unlines . flip map (nodes dag) $ \(i,n) ->
  let j = Internal (dagLevel dag) i
      name = case mr of
        Just (Var ret _) | ret == j -> last r
        _ -> dotId r j
  in dotNode r name n ++"\n"++
  (if typeNode n == UnknownType then name ++" [color=\"red\"]" else "")

dotPNode :: Label -> PNode -> String
dotPNode name (Dist f js _) =
  name ++" [shape=\"rectangle\" label=\""++ f ++"\"]\n"++
  dotConsts [name] js
dotPNode name (HODist f (Dist g js _) js' _) =
  name ++" [shape=\"rectangle\" label=\""++ f ++" "++ g ++"\"]\n"++
  dotConsts [name] (js ++ js')
dotPNode name (Loop sh (Lambda dag body) _) =
  unlines [dotConsts (r' ++ [dotId r' j]) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  "subgraph cluster_loop_"++ name ++" {\n"++ indent (
    "label=\"joint\"\n"++
    unlines [dotId r' j ++" [shape=\"rectangle\" label=\"index "++ show i ++"\"]"
            | (i,j) <- [1..] `zip` inputs dag] ++"\n"++
    dotDAG r' dag Nothing ++"\n"++
    dotPNode name body ++"\n"++
    name ++" [style=\"bold\"]"
  ) ++"\n}"
  where r' = [name]
dotPNode _ pn = error $ "dotPNode "++ show pn

dotPNodes :: Map Id PNode -> String
dotPNodes pn = unlines [dotPNode (dotId [] i) n | (i,n) <- Map.toList pn]

dotPBlock :: PBlock -> [NodeRef] -> String
dotPBlock pb@(PBlock block _ given _) rets =
  dotDAG [] (topDAG block) Nothing ++"\n"++
  --"subgraph cluster_program {\n"++ indent (
  --  "style=\"invis\"\n"++
    dotPNodes (pnodes pb) ++"\n"++
    unlines [dotId [] i ++" [style=\"bold\"]" | Var i _ <- rets] ++"\n"++
    unlines [dotId [] i ++" [style=\"filled\" fillcolor=\"gray\"]" | LVar i <- Map.keys given]
  --) ++"\n}"

edgeNodeRefs :: [Label] -> Label -> [NodeRef] -> String
edgeNodeRefs r name js = "{"++ unwords (do
  j <- js
  return $ case j of
    Var (Symbol s _) _ -> name ++ s
    Var i _ -> dotId r i
    Const c _ -> dotConst (r ++ [name]) c
  ) ++"} -> "++ name ++" [style=\"solid\"]"

edgeNodeRef :: [Label] -> Label -> NodeRef -> String
edgeNodeRef r name (Index j js) = edgeNodeRefs r name (j:js)

edgeNode :: [Label] -> Label -> Node -> String
edgeNode _ _ (Apply "getExternal" _ _) = ""
edgeNode r name (Apply "id" [j] _) = edgeNodeRef r name j
edgeNode r name (Apply f js _) = edgeNodeRefs r name js
edgeNode r name (Array sh (Lambda dag ret) _) =
  unlines [edgeNodeRefs r' (idx j) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  edgeDAG r' dag (Just ret)
  where r' = r ++ [name]
        idx j = if Just j == getId ret then name else dotId r' j
edgeNode r name (FoldScan _ _ (Lambda dag ret) seed ls _) =
  edgeNodeRefs r' elemt [ls] ++"\n"++
  edgeNodeRefs r' accum [seed] ++"\n"++
  edgeDAG r' dag (Just ret)
  where r' = r ++ [name]
        [i,j] = inputs dag
        elemt = if Just i == getId ret then name else dotId r' i
        accum = if Just j == getId ret then name else dotId r' j

edgeDAG :: [Label] -> DAG -> Maybe NodeRef -> String
edgeDAG r dag mr = unlines . flip map (nodes dag) $ \(i,n) ->
  let j = Internal (dagLevel dag) i
      name = case mr of
        Just (Var ret _) | ret == j -> last r
        _ -> dotId r j
  in edgeNode r name n

edgePNode :: Label -> PNode -> String
edgePNode name (Dist f js _) = edgeNodeRefs [] name js
edgePNode name (Loop sh (Lambda dag body) _) =
  unlines [edgeNodeRefs r' (dotId r' j) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  edgeDAG r' dag Nothing ++"\n"++
  edgePNode name body
  where r' = [name]
edgePNode name (HODist f n js _) = edgePNode name n ++"\n"++ edgeNodeRefs [] name js

edgePNodes :: Map Id PNode -> String
edgePNodes pn =
  unlines [x ++" -> "++ y ++" [style=\"invis\"]" | (x,y) <- pairs] ++"\n"++
  unlines [edgePNode (dotId [] i) n | (i,n) <- Map.toList pn]
  where names = [dotId [] i | (i,_) <- Map.toList pn]
        pairs = names `zip` tail names

edgePBlock :: PBlock -> String
edgePBlock pb@(PBlock block _ _ _) =
  edgeDAG [] (topDAG block) Nothing ++"\n"++
  edgePNodes (pnodes pb)

graphviz :: (ExprTuple t) => P t -> String
graphviz prog = "/*\n"++ showPBlock pb (show rets) ++"\n*/\n\n"++
  "strict digraph {\n"++ indent (
    "compound=\"true\"\n"++
    --"concentrate=\"true\"\n"++
    dotPBlock pb rets ++"\n"++
    edgePBlock pb
  ) ++"\n}"
  where (rets, pb) = isolateNodeRefPBlock f $ runProgExprs "ns" prog
        f Var{} = False
        f _ = True

vizIR :: (ExprTuple t) => P t -> IO String
vizIR = readProcess "dot" ["-Tsvg"] . graphviz
