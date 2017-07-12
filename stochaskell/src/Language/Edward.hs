module Language.Edward where

import Data.Expression hiding (const)
import Data.Expression.Const
import Data.List
import Data.Maybe
import Data.Program
import Data.Ratio
import Util

dtype :: Type -> String
dtype t | t == boolT = "tf.bool"
dtype IntT = "tf.int32"
dtype RealT = "tf.float32"

edId :: Id -> String
edId (Dummy    level i) =  "index_"++ show level ++"_"++ show i
edId (Volatile level i) = "sample_"++ show level ++"_"++ show i
edId (Internal level i) =  "value_"++ show level ++"_"++ show i

edNodeRef :: NodeRef -> String
edNodeRef (Var s _) = edId s
edNodeRef (Const (Exact a) IntT) = show . numerator $ toScalar a
edNodeRef (Const c _) = show c

edBuiltinFunctions =
  [("#>", "ed.dot")
  ]

edOperators =
  [("+",   "+")
  ]

edBuiltinDistributions =
  [("bernoulliLogits", ["Bernoulli", "logits"])
  ,("normal",          ["Normal",    "loc", "scale"])
  ,("normals",         ["Normal",    "loc", "scale"])
  ,("uniforms",        ["Uniform"])
  ]

edNode :: [PNode] -> Label -> Node -> String
edNode r _ (Apply "getExternal" [Var i@(Volatile 0 k) _] _) =
  edPNode (edId i) (r!!k)
edNode _ name (Apply "asVector" [j] _) = name ++" = "++ edNodeRef j
edNode _ name (Apply "asMatrix" [j] _) = name ++" = "++ edNodeRef j
edNode _ name (Apply op [i,j] _) | s /= "" =
  name ++" = "++ edNodeRef i ++" "++ s ++" "++ edNodeRef j
  where s = fromMaybe "" $ lookup op edOperators
edNode _ name (Apply f js _) =
  name ++" = "++ s ++"("++ edNodeRef `commas` js ++")"
  where s = fromMaybe f (lookup f edBuiltinFunctions)
edNode r name (Array sh dag ret (ArrayT _ _ t))
  | edDAG r dag /= "" = -- TODO: or ret depends on index
    "def "++ fn ++":\n"++
      edDAG r dag ++"\n  "++
      "return "++ edNodeRef ret ++"\n"++
    name ++" = tf.stack("++ go (inputs dag) sh ++")"
  | otherwise =
    name ++" = "++ edNodeRef ret ++" * tf.ones(["++
      edNodeRef `commas` map snd sh ++"], dtype="++ dtype t ++")"
  where fn = name ++"_fn("++ edId `commas` inputs dag ++")"
        go [] [] = fn
        go (i:is) ((a,b):sh) =
          "["++ go is sh ++" for "++ edId i ++" in xrange("++
            edNodeRef a ++", "++ edNodeRef b ++"+1)]"
edNode _ _ n = error $ "edNode "++ show n

edPNode :: Label -> PNode -> String
edPNode name (Dist f args _) | lookup f edBuiltinDistributions /= Nothing =
  name ++" = "++ c ++ "("++ ps ++")"
  where c:params = fromJust $ lookup f edBuiltinDistributions
        h p a = p ++"="++ edNodeRef a
        ps | null params = edNodeRef `commas` args
           | otherwise = intercalate ", " (zipWith h params args)

edDAG :: [PNode] -> DAG -> String
edDAG r dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = edId $ Internal (dagLevel dag) i
  in edNode r name n

edProgram :: (ExprTuple t) => Prog t -> String
edProgram prog = edDAG (reverse refs) (head block)
  where (rets, (PBlock block refs given)) = runProgExprs prog
