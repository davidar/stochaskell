module Language.Edward where

import Control.Monad
import Data.Either.Utils
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Expression.Const.IO
import Data.Expression.Eval
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Program
import qualified Data.Set as Set
import System.Directory
import System.IO.Temp
import System.Process
import Util

dtype :: Type -> String
dtype t | t == boolT = "tf.bool"
dtype IntT = "tf.int32"
dtype RealT = "tf.float32"
dtype (ArrayT _ _ t) = dtype t

edId :: Id -> String
edId = show

edNodeRef :: NodeRef -> String
edNodeRef (Var s _) = edId s
edNodeRef (Const c RealT) = show $ real c
edNodeRef (Const c _) = show c
edNodeRef (Index (Var f (ArrayT _ sh _)) js) =
  edId f ++"["++ intercalate "," (zipWith g (reverse js) (map fst sh)) ++"]"
  where g i l = edNodeRef i ++"-"++ edNodeRef l

edBuiltinFunctions =
  [("#>",       "ed.dot")
  ,("**",       "tf.pow")
  ,("<>",       "tf.matmul")
  ,("inv",      "tf.matrix_inverse")
  ,("chol",     "tf.cholesky")
  ,("sqrt",     "tf.sqrt")
  ,("tr'",      "tf.matrix_transpose")
  ,("exp",      "tf.exp")
  ,("diag",     "tf.diag")
  ,("==",       "all_equal")
  ,("asColumn", "ascolumn")
  ,("asRow",    "asrow")
  ]

edOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   "*")
  ,("*>",  "*")
  ,("/",   "/")
  ]

edBuiltinDistributions =
  [("bernoulliLogits", ["Bernoulli",    "logits"])
  ,("inv_gamma",       ["InverseGamma", "concentration", "rate"])
  ,("multi_normal",    ["MultivariateNormalFullCovariance", "loc", "covariance_matrix"])
  ,("normal",          ["Normal",       "loc", "scale"])
  ,("normals",         ["Normal",       "loc", "scale"])
  ,("uniforms",        ["Uniform"])
  ]

edPrelude :: String
edPrelude = unlines
  ["import sys"
  ,"from collections import OrderedDict"
  ,"import edward as ed"
  ,"import numpy as np"
  ,"import tensorflow as tf"
  ,""
  ,"def all_equal(x,y): return tf.reduce_all(tf.equal(x,y))"
  ,"def ascolumn(a): return tf.reshape(a, (-1, 1))"
  ,"def asrow(a):    return tf.reshape(a, ( 1,-1))"
  ]

edNode :: Map Id PNode -> Label -> Node -> String
edNode r _ (Apply "getExternal" [Var i t] _) =
  case Map.lookup i r of
    Just n  -> edPNode       (edId i) n
    Nothing -> edPlaceholder (edId i) t
edNode _ name (Apply "ifThenElse" [a,b,c] _) =
  name ++" = tf.cond("++ edNodeRef a ++", lambda: "++ edNodeRef b
                                     ++", lambda: "++ edNodeRef c ++")"
edNode _ name (Apply op [i,j] _) | s /= "" =
  name ++" = "++ edNodeRef i ++" "++ s ++" "++ edNodeRef j
  where s = fromMaybe "" $ lookup op edOperators
edNode _ name a@(Apply f js _)
  | s /= "" = name ++" = "++ s ++"("++ edNodeRef `commas` js ++")"
  | otherwise = error $ "edNode "++ show a
  where s = fromMaybe "" (lookup f edBuiltinFunctions)
edNode r name (Array sh (Lambda dag ret) (ArrayT _ _ t))
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
edPNode name d@(Dist f args t)
  | lookup f edBuiltinDistributions /= Nothing =
  name ++" = ed.models."++ c ++ "("++ ps ++")\n"++
  "dim_"++ name ++" = ["++ g `commas` typeDims t ++"]"
  | otherwise = error $ "edPNode "++ show d
  where c:params = fromJust $ lookup f edBuiltinDistributions
        h p a = p ++"="++ edNodeRef a
        ps | null params = edNodeRef `commas` args
           | otherwise = intercalate ", " (zipWith h params args)
        g (a,b) = edNodeRef b ++"-"++ edNodeRef a ++"+1"

edPlaceholder :: Label -> Type -> String
edPlaceholder name t =
  name ++" = tf.Variable(np.load('"++ name ++".npy'), "++
                        "trainable=False, dtype="++ dtype t ++")"

edDAG :: Map Id PNode -> DAG -> String
edDAG r dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = edId $ Internal (dagLevel dag) i
  in edNode r name n

edProgram :: (ExprTuple t) => Int -> Int -> Double -> Prog t -> Maybe t -> String
edProgram numSamples numSteps stepSize prog init =
  edPrelude ++"\n"++
  "if True:\n"++
    edDAG pn (topDAG block) ++"\n"++
  "latent = "++ latent ++"\n"++
  "data = "++ printedConds ++"\n"++
  "inference = ed.HMC(latent, data)\n"++
  "stdout = sys.stdout; sys.stdout = sys.stderr\n"++
  "inference.run(step_size="++ show stepSize ++
               ",n_steps="++ show numSteps ++")\n"++
  "sys.stdout = stdout\n"++
  "print(map(list, zip(*[q.params.eval().tolist() for q in latent.values()])))"
  where (_, pb@(PBlock block _ given _)) = runProgExprs "ed" prog
        skel = modelSkeleton pb
        pn = Map.filterWithKey (const . (`Set.member` skel)) $ pnodes pb
        latent = "OrderedDict(["++ g `commas` (Map.keys pn \\ [k | LVar k <- Map.keys given]) ++"])"
        g i | isJust init = pre ++"np.load('"++ edId i ++".npy')*tf.ones"++ post
            | otherwise = pre ++"tf.zeros"++ post
          where pre = "("++ edId i ++", ed.models.Empirical(params=tf.Variable("
                post = "(["++ show numSamples ++"] + dim_"++ edId i ++"))))"
        printedConds = "{"++ intercalate ", "
          [edId k ++": np.load('"++ edId k ++".npy')"
          | LVar k <- Map.keys given, k `Set.member` skel] ++"}"

hmcEdward :: (ExprTuple t, Read t) => Int -> Int -> Double -> Prog t -> Maybe t -> IO [t]
hmcEdward numSamples numSteps stepSize prog init =
  withSystemTempDirectory "edward" $ \tmpDir -> do
    pwd <- getCurrentDirectory
    setCurrentDirectory tmpDir
    when (isJust init) $
      dump $ unifyTuple block rets (fromJust init) given
    dump given
    let python = pwd ++"/edward/env/bin/python"
    --callProcess python []
    out <- readProcess python [] $
      edProgram numSamples numSteps stepSize prog init
    setCurrentDirectory pwd
    let vals = zipWith reshape lShapes <$> read out
    return [let env = Map.fromList [(LVar i, x)
                                   | ((i,d),x) <- Map.toAscList latents `zip` xs]
            in fromRight $ evalPBlock pb rets env
           | xs <- vals]
  where (rets, pb@(PBlock block _ given _)) = runProgExprs "ed" prog
        dump env = forM_ (Map.toList env) $ \(LVar i,c) -> do
                     writeNPy (edId i ++".npy") c
        latents = pnodes pb Map.\\ Map.fromList [(k,v) | (LVar k,v) <- Map.toList given]
        lShapes = evalShape given block . typeDims . typePNode <$> Map.elems latents
