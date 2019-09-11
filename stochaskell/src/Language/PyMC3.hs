{-# LANGUAGE RecordWildCards #-}
{-|
Description : PyMC3 integration
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.PyMC3
  ( PyMC3Inference(..), PyMC3Step(..)
  , defaultPyMC3Inference
  , runPyMC3
  , pmProgram, pmProgram'
  ) where

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

pmId :: Id -> String
pmId = show

pmNodeRef :: NodeRef -> String
pmNodeRef (Var s _) = pmId s
pmNodeRef (Const c RealT) = show $ real c
pmNodeRef (Const c _) = show c
pmNodeRef (Index (Var f (ArrayT _ sh _)) js) =
  pmId f ++"["++ intercalate "," (zipWith g (reverse js) (map fst sh)) ++"]"
  where g i l = pmNodeRef i ++"-"++ pmNodeRef l

pmNodeRef' :: NodeRef -> String
pmNodeRef' (Var s IntT) = "int("++ pmId s ++".eval())"
pmNodeRef' r = pmNodeRef r

pmBuiltinFunctions =
  [("inv",      "pm.math.matrix_inverse")
  ,("sqrt",     "pm.math.sqrt")
  ,("exp",      "pm.math.exp")
  ,("diag",     "theano.tensor.basic.diag")
  ,("chol",     "theano.tensor.slinalg.cholesky")
  ,("asColumn", "ascolumn")
  ,("asRow",    "asrow")
  ,("negate",   "-")
  ,("==",       "tt.eq")
  ,("ifThenElse", "ifelse")
  ,("quad_form_diag", "quad_form_diag")
  ]

pmOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   "*")
  ,("*>",  "*")
  ,("/",   "/")
  ,("**",  "**")
  ]

pmBuiltinDistributions =
  [("bernoulli",       ["Bernoulli",    "p"])
  ,("cauchy",          ["Cauchy",       "alpha", "beta"])
  ,("cauchys",         ["Cauchy",       "alpha", "beta"])
  ,("gamma",           ["Gamma",        "alpha", "beta"])
  ,("inv_gamma",       ["InverseGamma", "alpha", "beta"])
  ,("multi_normal",    ["MvNormal",     "mu", "cov"])
  ,("normal",          ["Normal",       "mu", "sd"])
  ,("normals",         ["Normal",       "mu", "sd"])
  ,("uniform",         ["Uniform",      "lower", "upper"])
  ,("uniforms",        ["Uniform",      "lower", "upper"])
  ,("wishart",         ["Wishart",      "nu", "V"])
  ]

pmPrelude :: String
pmPrelude = unlines
  ["import numpy as np"
  ,"import pymc3 as pm"
  ,"import sys"
  ,"import theano"
  ,"import theano.tensor as tt"
  ,"import theano.tensor.basic"
  ,"import theano.tensor.slinalg"
  ,"from theano.ifelse import ifelse"
--  ,"from memory_profiler import profile"
  ,""
  ,"def ascolumn(a): return a.dimshuffle(0,'x')"
  ,"def asrow(a):    return a.dimshuffle('x',0)"
  ,"def quad_form_diag(m,v):"
  ,"  d = theano.tensor.basic.diag(v)"
  ,"  return d.dot(m).dot(d)"
  ,"def lkj_corr(name, eta, n):"
  ,"  C_triu = pm.LKJCorr(name + '_triu', eta=eta, n=n)"
  ,"  shape = n * (n - 1) // 2"
  ,"  tri_index = np.zeros([n, n], dtype='int32')"
  ,"  tri_index[np.triu_indices(n, k=1)] = np.arange(shape)"
  ,"  tri_index[np.triu_indices(n, k=1)[::-1]] = np.arange(shape)"
  ,"  return pm.Deterministic(name, tt.fill_diagonal(C_triu[tri_index], 1))"
  ,""
  ,"theano.config.floatX = 'float32'"
  ]

pmNode :: (Map Id PNode, Env) -> Label -> Node -> String
pmNode (r,given) _ (Apply "getExternal" [Var i t] _) =
  case Map.lookup i r of
    _ | idLevel i > 0 -> ""
    Just n -> pmPNode (pmId i) n (Map.lookup (LVar i) given)
    Nothing -> pmId i ++" = tt.as_tensor_variable(np.load('"++ pmId i ++".npy').astype('float32'))"
pmNode _ name (Apply "tr'" [a] _) =
  name ++" = "++ pmNodeRef a ++".T"
pmNode _ name (Apply op [i,j] _) | op == "#>" || op == "<>" =
  name ++" = "++ pmNodeRef i ++".dot("++ pmNodeRef j ++")"
pmNode _ name (Apply op [i,j] _) | s /= "" =
  name ++" = "++ pmNodeRef i ++" "++ s ++" "++ pmNodeRef j
  where s = fromMaybe "" $ lookup op pmOperators
pmNode _ name (Apply f js _) | s /= "" =
  name ++" = "++ s ++"("++ pmNodeRef `commas` js ++")"
  where s = fromMaybe "" $ lookup f pmBuiltinFunctions
pmNode r name (Array sh (Lambda dag ret) _)
  | pmDAG r dag /= "" = -- TODO: or ret depends on index
    pmArray name (inputs dag) sh (pmDAG r dag) (pmNodeRef ret)
  | otherwise = name ++" = "++ pmNodeRef ret ++" * np.ones(("++
                  pmNodeRef' `commas` map snd sh ++"), dtype='float32')"
pmNode _ _ n = error $ "pmNode "++ show n

pmPNode :: Label -> PNode -> Maybe ConstVal -> String
pmPNode name (Dist "bernoulliLogit" [l] t) val =
  pmPNode' name "bernoulli" ["pm.invlogit("++ pmNodeRef l ++")"] t val
pmPNode name (Dist "bernoulliLogits" args t) val =
  pmPNode name (Dist "bernoulliLogit" args t) val
pmPNode name (Dist "lkj_corr" [v] (ArrayT _ [(Const 1 _, Const n _),_] _)) Nothing =
  name ++" = lkj_corr('"++ name ++"', eta="++ pmNodeRef v ++", n="++ show n ++")"
pmPNode name (Dist f args t) val =
  pmPNode' name f (map pmNodeRef args) t val
pmPNode name (Loop sh (Lambda dag (Dist d args _)) t) val
  | not (any p $ nodes dag), not (varies dag args)
  = pmPNode name (Dist d args t) val
  where p (_, Apply "getExternal" _ _) = False
        p _ = True
pmPNode name (Loop sh (Lambda defs dist) _) Nothing =
  pmLoop name (inputs defs) sh (pmDAG (Map.empty, emptyEnv) defs) dist
pmPNode name (HODist "orderedSample" d [n] _) Nothing =
  pmLoop name [Dummy 1 1] [(Const 1 IntT,n)] "" d
pmPNode _ n _ = error $ "pmPNode "++ show n

pmArray :: Label -> [Id] -> [(NodeRef,NodeRef)] -> String -> String -> String
pmArray name params sh context ret =
  "def "++ fn ++":\n"++
    context ++"\n  "++
    "return "++ ret ++"\n"++
  name ++" = pm.Deterministic('"++ name ++"', "++
    "tt.stack("++ go params sh ++"))"
  where fn = name ++"_fn("++ pmId `commas` params ++")"
        go [] [] = fn
        go (i:is) ((a,b):sh) =
          "["++ go is sh ++" for "++ pmId i ++" in range("++
            pmNodeRef' a ++", "++ pmNodeRef' b ++"+1)]"

pmLoop :: Label -> [Id] -> [(NodeRef,NodeRef)] -> String -> PNode -> String
pmLoop name params sh context dist = pmArray name params sh context ret
  where name' = name ++ concat ["_' + str("++ pmId i ++") + '" | i <- params]
        ret = fromJust . stripPrefix (name' ++" = ") $
          pmPNode name' dist Nothing

pmPNode' :: Label -> String -> [String] -> Type -> Maybe ConstVal -> String
pmPNode' name f args t val | isJust (lookup f pmBuiltinDistributions) =
  name ++" = "++ ctor ++ "('"++ name ++"', "++ ps ++", "++ obs ++
    "shape=("++ g `commas` typeDims t ++"))"
  where c:params = fromJust $ lookup f pmBuiltinDistributions
        h p a = p ++"="++ a
        ps = intercalate ", " (zipWith h params args)
        g (a,b) = pmNodeRef' b ++"-"++ pmNodeRef' a ++"+1"
        obs | isNothing val = ""
            | otherwise = "observed=np.load('"++ name ++".npy').astype('float32'), "
        ctor | (SubrangeT _ lo hi) <- t, isNothing val =
               let kwargs = case (lo,hi) of
                     (Just a, Just b)  -> "lower="++ pmNodeRef a ++
                                        ", upper="++ pmNodeRef b
                     (Just a, Nothing) -> "lower="++ pmNodeRef a
                     (Nothing, Just b) -> "upper="++ pmNodeRef b
                     (Nothing, Nothing) -> error "invalid subrange"
               in "pm.Bound(pm."++ c ++", "++ kwargs ++")"
             | otherwise = "pm."++ c
pmPNode' _ f _ _ _ = error $ "pmPNode' "++ f

pmDAG :: (Map Id PNode, Env) -> DAG -> String
pmDAG r dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = pmId $ Internal (dagLevel dag) i
  in pmNode r name n

-- | generate PyMC3 code, as used by 'runPyMC3'
pmProgram :: (ExprTuple t) => P t -> String
pmProgram prog =
  pmPrelude ++"\n"++
  --"@profile(stream=sys.stderr)\n"++
  "with pm.Model() as model:\n"++
    pmDAG (pn, Map.map fst given) (topDAG block)
  where pb@(PBlock block _ given _) = snd $ runProgExprs "pm" prog
        skel = modelSkeleton pb
        pn = Map.filterWithKey (const . (`Set.member` skel)) $ pnodes pb

pmEnv :: Env -> String
pmEnv env | Map.null env = "None"
pmEnv env = "{"++ g `commas` [k | LVar k <- Map.keys env] ++"}"
  where g i = "'"++ pmId i ++"': np.load('"++ pmId i ++".npy').astype('float32')"

data PyMC3Inference
  = PyMC3Sample
    { pmDraws :: Int
    , pmStep :: Maybe PyMC3Step
    , pmInit :: Maybe String
    , pmStart :: Env
    , pmTune :: Int
    }

instance Show PyMC3Inference where
  show PyMC3Sample{..} = "pm.sample(draws="++ show pmDraws
                                ++",step="++ maybe "None" show pmStep
                                ++",init="++ maybe "None" show pmInit
                                ++",start="++ pmEnv pmStart
                                ++",tune="++ show pmTune
                                ++")"

data PyMC3Step
  = NUTS
  | Metropolis
  | Slice
  | HamiltonianMC
    { pathLength :: Double
    , stepRand :: String
    , stepScale :: Double
    }

instance Show PyMC3Step where
  show NUTS = "pm.NUTS()"
  show Metropolis = "pm.Metropolis()"
  show Slice = "pm.Slice()"
  show HamiltonianMC{..} = "pm.HamiltonianMC(path_length="++ show pathLength
                                         ++",step_rand="++ stepRand
                                         ++",step_scale="++ show stepScale
                                         ++")"

-- | default PyMC3 configuration, see https://docs.pymc.io/api/inference.html
defaultPyMC3Inference :: PyMC3Inference
defaultPyMC3Inference = PyMC3Sample
  { pmDraws = 500
  , pmStep = Nothing
  , pmInit = Just "auto"
  , pmStart = emptyEnv
  , pmTune = 500
  }

pmProgram' :: (ExprTuple t) => PyMC3Inference -> P t -> Maybe t -> String
pmProgram' sample prog init =
  pmProgram prog ++"\n\n  "++
  "trace = "++ show sample{pmStart = initEnv "pm" prog init Map.\\ given} ++"\n  "++
  "print(map(list, zip("++ g `commas` Map.keys (latentPNodes pb) ++")))"
  where (rets, pb@(PBlock block _ given _)) = runProgExprs "pm" prog
        g i = "trace['"++ pmId i ++"'].tolist()"

-- | perform inference via the PyMC3 code generation backend
runPyMC3 :: (ExprTuple t, Read t) => PyMC3Inference -> P t -> Maybe t -> IO [t]
runPyMC3 sample prog init = withSystemTempDirectory "pymc3" $ \tmpDir -> do
  forM_ (Map.toList $ initEnv "pm" prog init) $ \(LVar i,c) ->
    writeNPy (tmpDir ++"/"++ pmId i ++".npy") c
  writeFile (tmpDir ++"/main.py") $ pmProgram' sample prog init
  pwd <- getCurrentDirectory
  let python = pwd ++"/pymc3/env/bin/python"
  out <- flip readCreateProcess "" $ (proc python ["main.py"]) { cwd = Just tmpDir }
  let vals = zipWith reshape lShapes <$> read out
  return [let env = Map.fromList [(LVar i, x)
                                 | ((i,d),x) <- Map.toAscList latents `zip` xs]
          in fromRight $ evalPBlock pb rets env
         | xs <- vals]
  where (rets, pb@(PBlock block _ given _)) = runProgExprs "pm" prog
        latents = latentPNodes pb
        lShapes = evalShape (Map.map fst given) block . typeDims . typePNode <$> Map.elems latents
