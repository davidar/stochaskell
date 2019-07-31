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
dotConsts r js =
  unlines [dotConst r c ++" [shape=\"plain\" label=\""++ show c ++"\"]" | Const c _ <- js]

dotNodeRef :: [Label] -> Label -> NodeRef -> String
dotNodeRef r name Index{} = name ++" [label=\"!\"]"

dotNode :: [Label] -> Label -> Node -> String
dotNode _ _ (Apply "getExternal" _ _) = ""
dotNode r name (Apply "id" [j] _) = dotNodeRef r name j
dotNode r name (Apply f js _) = name ++" [label=\""++ f ++"\"]\n"++ dotConsts (r ++ [name]) js
dotNode r name (Array sh (Lambda dag ret) _) =
  unlines [dotConsts (r' ++ [dotId r' j]) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  "subgraph cluster_array_"++ name ++" {\n"++ indent (
    "label=\"array\"\n"++
    unlines [dotId r' j ++" [shape=\"rectangle\" label=\"index "++ show i ++"\"]"
            | (i,j) <- [1..] `zip` inputs dag] ++"\n"++
    dotDAG r' dag (Just ret) ++"\n"++
    name ++ sret ++"style=\"bold\"]"
  ) ++"\n}"
  where r' = r ++ [name]
        sret = case ret of
          Var i _ -> " ["
          Const c _ -> " [label=\""++ show c ++"\" "

dotDAG :: [Label] -> DAG -> Maybe NodeRef -> String
dotDAG r dag mr = unlines . flip map (nodes dag) $ \(i,n) ->
  let j = Internal (dagLevel dag) i
      name = case mr of
        Just (Var ret _) | ret == j -> last r
        _ -> dotId r j
  in dotNode r name n

dotPNode :: Label -> PNode -> String
dotPNode name (Dist f js _) =
  name ++" [shape=\"rectangle\" label=\""++ f ++"\"]\n"++
  dotConsts [name] js
dotPNode name (HODist f (Dist g js _) js' _) =
  name ++" [shape=\"rectangle\" label=\""++ f ++" "++ g ++"\"]\n"++
  dotConsts [name] (js ++ js')

dotPNodes :: Map Id PNode -> String
dotPNodes pn = unlines [dotPNode (dotId [] i) n | (i,n) <- Map.toList pn]

dotPBlock :: PBlock -> [NodeRef] -> String
dotPBlock pb@(PBlock block _ _ _) rets =
  dotDAG [] (topDAG block) Nothing ++"\n"++
  "subgraph cluster_program {\n"++ indent (
    "style=\"invis\"\n"++
    dotPNodes (pnodes pb) ++"\n"++
    unlines [dotId [] i ++" [style=\"bold\"]" | Var i _ <- rets]
  ) ++"\n}"

edgeNodeRefs :: [Label] -> Label -> [NodeRef] -> String
edgeNodeRefs r name js = "{"++ unwords (do
  j <- js
  return $ case j of
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
  unlines [edgeNodeRefs r' (dotId r' j) [lo,hi]
          | (j,(lo,hi)) <- inputs dag `zip` sh] ++"\n"++
  edgeDAG r' dag (Just ret)
  where r' = r ++ [name]

edgeDAG :: [Label] -> DAG -> Maybe NodeRef -> String
edgeDAG r dag mr = unlines . flip map (nodes dag) $ \(i,n) ->
  let j = Internal (dagLevel dag) i
      name = case mr of
        Just (Var ret _) | ret == j -> last r
        _ -> dotId r j
  in edgeNode r name n

edgePNode :: Label -> PNode -> String
edgePNode name (Dist f js _) = edgeNodeRefs [] name js
edgePNode name (HODist f n js _) = edgePNode name n ++"\n"++ edgeNodeRefs [] name js
edgePNode _ _ = ""

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
    dotPBlock pb rets ++"\n"++
    edgePBlock pb
  ) ++"\n}"
  where (rets, pb) = isolateNodeRefPBlock f $ runProgExprs "ns" prog
        f Var{} = False
        f _ = True

vizIR :: (ExprTuple t) => P t -> IO String
vizIR = readProcess "dot" ["-Tsvg"] . graphviz
