module Language.PyMC3 where

import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Expression.Eval
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Program
import Data.Ratio
import System.Directory
import System.Process
import Util

pmId :: Id -> String
pmId (Dummy    level i) =  "index_"++ show level ++"_"++ show i
pmId (Volatile level i) = "sample_"++ show level ++"_"++ show i
pmId (Internal level i) =  "value_"++ show level ++"_"++ show i

pmNodeRef :: NodeRef -> String
pmNodeRef (Var s _) = pmId s
pmNodeRef (Const (Exact a) IntT) = show . numerator $ toScalar a
pmNodeRef (Const c _) = show c

pmBuiltinFunctions =
  []

pmOperators =
  [("+",   "+")
  ]

pmBuiltinDistributions =
  [("bernoulli",       ["Bernoulli", "p"])
  ,("normal",          ["Normal",    "mu", "sd"])
  ,("normals",         ["Normal",    "mu", "sd"])
  ,("uniforms",        ["Uniform",   "lower", "upper"])
  ]

pmPrelude :: String
pmPrelude = unlines
  ["import numpy as np"
  ,"import pymc3 as pm"
  ]

pmNode :: ([PNode],Env) -> Label -> Node -> String
pmNode (r,given) _ (Apply "getExternal" [Var i@(Volatile 0 k) _] _) =
  pmPNode (pmId i) (r!!k) (Map.lookup i given)
pmNode _ name (Apply "asVector" [j] _) = name ++" = "++ pmNodeRef j
pmNode _ name (Apply "asMatrix" [j] _) = name ++" = "++ pmNodeRef j
pmNode _ name (Apply "#>" [i,j] _) =
  name ++" = "++ pmNodeRef i ++".dot("++ pmNodeRef j ++")"
pmNode _ name (Apply op [i,j] _) | s /= "" =
  name ++" = "++ pmNodeRef i ++" "++ s ++" "++ pmNodeRef j
  where s = fromMaybe "" $ lookup op pmOperators
pmNode _ name (Apply f js _) =
  name ++" = "++ s ++"("++ pmNodeRef `commas` js ++")"
  where s = fromMaybe f (lookup f pmBuiltinFunctions)
pmNode r name (Array sh dag ret (ArrayT _ _ t))
  | pmDAG r dag /= "" = undefined -- TODO: or ret depends on index
  | otherwise = name ++" = "++ pmNodeRef ret ++" * np.ones(("++
                  pmNodeRef `commas` map snd sh ++"))"

pmPNode :: Label -> PNode -> Maybe ConstVal -> String
pmPNode name (Dist "bernoulliLogit" [l] t) =
  pmPNode' name "bernoulli" ["pm.invlogit("++ pmNodeRef l ++")"] t
pmPNode name (Dist "bernoulliLogits" args t) =
  pmPNode name (Dist "bernoulliLogit" args t)
pmPNode name (Dist f args t) = pmPNode' name f (map pmNodeRef args) t

pmPNode' :: Label -> String -> [String] -> Type -> Maybe ConstVal -> String
pmPNode' name f args t val | lookup f pmBuiltinDistributions /= Nothing =
  name ++" = pm."++ c ++ "('"++ name ++"', "++ ps ++", "++ obs ++
    "shape=("++ g `commas` typeDims t ++"))"
  where c:params = fromJust $ lookup f pmBuiltinDistributions
        h p a = p ++"="++ a
        ps = intercalate ", " (zipWith h params args)
        g (a,b) = pmNodeRef b ++"-"++ pmNodeRef a ++"+1"
        obs | val == Nothing = ""
            | otherwise = "observed=np.array("++ show (fromJust val) ++"), "

pmDAG :: ([PNode],Env) -> DAG -> String
pmDAG r dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = pmId $ Internal (dagLevel dag) i
  in pmNode r name n

pmProgram :: (ExprTuple t) => Int -> Prog t -> String
pmProgram numSamples prog =
  pmPrelude ++"\n"++
  "with pm.Model() as model:\n"++
    pmDAG (reverse refs, given) (head block) ++"\n  "++
    "trace = pm.sample("++ show numSamples ++")\n  "++
    "print(zip("++ g `commas` rets ++"))"
  where (rets, (PBlock block refs given)) = runProgExprs prog
        g r = "trace['"++ pmNodeRef r ++"'].tolist()"

hmcPyMC3 :: (ExprTuple t, Read t) => Int -> Prog t -> IO [t]
hmcPyMC3 numSamples prog = do
  pwd <- getCurrentDirectory
  out <- readProcess (pwd ++"/pymc3/env/bin/python") [] $
    pmProgram numSamples prog
  return (read out)
