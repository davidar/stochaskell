{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables,
             FlexibleContexts, TypeFamilies, MultiParamTypeClasses,
             MonadComprehensions, GeneralizedNewtypeDeriving, LambdaCase #-}
module Data.Program where

import Prelude hiding (isInfinite)

import Control.DeepSeq
import Control.Exception
import qualified Control.Monad as Monad
import Control.Monad.Guard
import Control.Monad.State hiding (guard)
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Expression hiding (const,foldl,foldr,scanl,scanr)
import qualified Data.Expression as E
import Data.Expression.Case
import Data.Expression.Const
import Data.Expression.Eval
import Data.Expression.Extract
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Number.LogFloat as LF
import Data.Number.Transfinite hiding (log)
import qualified Data.Random as Rand
import Data.Random.Distribution (logPdf)
import Data.Random.Distribution.Abstract
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Exts hiding ((<#))
import Numeric.SpecFunctions
import Util


------------------------------------------------------------------------------
-- PROGRAMS                                                                 --
------------------------------------------------------------------------------

data PNode = Dist { dName :: String
                  , dArgs :: [NodeRef]
                  , typePNode :: Type
                  }
           | Loop { lShape :: [Interval NodeRef]
                  , lFunc  :: Lambda PNode
                  , typePNode :: Type
                  }
           | HODist { dName :: String
                    , dArg0 :: PNode
                    , dArgs :: [NodeRef]
                    , typePNode :: Type
                    }
{-
           | ITDist { dDefs :: DAG
                    , dBase :: [PNode]
                    , dRets :: [NodeRef]
                    , dInvF :: [DExpr]
                    , dInvJ :: [[DExpr]] -- TODO: just the determinant
                    , typePNode :: Type
                    }
-}
           | Switch { sHead :: NodeRef
                    , sAlts :: [(Lambda [NodeRef], [PNode])]
                    , sNS :: NS
                    , typePNode :: Type
                    }
           | Chain  { cRange :: Interval NodeRef
                    , cAux :: [PNode]
                    , cFunc :: Lambda NodeRef
                    , cInit :: NodeRef
                    , cNS :: NS
                    , typePNode :: Type
                    }
           | UnfoldP
                    { uAux :: [PNode]
                    , uFunc :: Lambda NodeRef
                    , uSeed :: NodeRef
                    , uNS :: NS
                    , typePNode :: Type
                    }
           deriving (Eq)

dependsPNode :: Block -> PNode -> Set Id
dependsPNode block (Dist _ args _) =
  Set.unions $ map (dependsNodeRef block) args
dependsPNode block (HODist _ arg0 args _) =
  Set.unions $ dependsPNode block arg0 : map (dependsNodeRef block) args
dependsPNode block (Loop sh (Lambda defs dist) _) =
  Set.unions $ map (d . fst) sh ++ map (d . snd) sh ++ [ddeps]
  where d = dependsNodeRef block
        ddeps = Set.filter ((dagLevel defs >) . idLevel) $
          dependsPNode (deriveBlock defs block) dist

extractPNode :: (NodeRef -> State Block NodeRef) -> (Node -> State Block NodeRef)
             -> EEnv -> Block -> PNode -> State Block PNode
extractPNode fNodeRef fNode env block = go where
  go (Dist name args t) = do
    args' <- sequence $ goRef <$> args
    t' <- extractType env block t
    return (Dist name args' t')
  go (Loop sh lam t) = do
      let (lo,hi) = unzip sh
      lo' <- sequence $ goRef <$> lo
      hi' <- sequence $ goRef <$> hi
      let sh' = zip lo' hi'
      lam' <- extractLambdaPNode lam
      t' <- extractType env block t
      return $ Loop sh' lam' t'
  go (HODist name arg0 args t) = do
    arg0' <- go arg0
    args' <- sequence $ extractNodeRef fNodeRef fNode env block <$> args
    t' <- extractType env block t
    return (HODist name arg0' args' t')
  go pn = error $ "extractPNode "++ show pn
  goRef = extractNodeRef fNodeRef fNode env block
  extractLambdaPNode (Lambda body pn) = do
    d <- getNextLevel
    let inps = inputsLevel d body
        ids' = f <$> inps
        env' = bindInputs body ids' `unionEEnv` env
    runLambda inps $ extractPNode fNodeRef fNode env' block' pn
    where f (i,t) = DExpr . return $ Var i t
          block' = deriveBlock body block

isolateNodeRefPBlock :: (NodeRef -> Bool) -> ([NodeRef], PBlock) -> ([NodeRef], PBlock)
isolateNodeRefPBlock p (rets, PBlock block acts given ns) =
  (rets', PBlock block' acts' given ns)
  where g r | p r = simplify $ Apply "id" [r] (typeRef r)
            | otherwise = simplifyNodeRef r
        ((acts', rets'), block') = flip runState emptyBlock $ do
          acts' <- sequence $ extractPNode   g simplify emptyEEnv block <$> acts
          rets' <- sequence $ extractNodeRef g simplify emptyEEnv block <$> rets
          return (acts', rets')

instance Show PNode where
  show (Dist d js t) = unwords (d : map show js) ++" :: P "++ show t
  show (Loop sh (Lambda dag hd) t) = "\n"++
    "  [ "++ (drop 4 . indent . indent $ showLet dag hd) ++"\n"++
    "  | "++ intercalate ", " (zipWith g (inputs dag) sh) ++" ] :: "++ show t
    where g i (a,b) = show i ++" <- "++ show a ++"..."++ show b
  show (HODist d j0 js t) = unwords (d : ("("++ show j0 ++")") : map show js) ++" :: P "++ show t
  show (Switch e alts ns _) = "switch "++ show e ++" of\n"++ indent cases
    where cases = unlines $ do
            (i, (Lambda dag ret, refs)) <- zip [0..] alts
            let lhs | typeRef e == IntT = show (i+1)
                    | otherwise = "C"++ show i ++" "++ unwords (map show $ inputs dag)
                rhs = indent . showLet' dag . showPNodes ns (dagLevel dag) refs $ show ret
            return $ lhs ++" ->\n"++ rhs
  show (Chain range refs (Lambda dag ret) x ns _) =
    "chain "++ show range ++" "++ show x ++" $ \\"++ show i ++" "++ show j ++" ->\n"++
      (indent . showLet' dag . showPNodes ns (dagLevel dag) refs $ show ret)
    where [i,j] = inputs' dag
  show (UnfoldP refs (Lambda dag ret) seed ns _) =
    "unfoldP "++ show seed ++" $ \\"++ show i ++" ->\n"++
      (indent . showLet' dag . showPNodes ns (dagLevel dag) refs $ show ret)
    where [i] = inputs' dag

data PBlock = PBlock { definitions :: Block
                     , actions     :: [PNode]
                     , constraints :: Map LVal (ConstVal, Type)
                     , namespace   :: String
                     }
            deriving (Eq)
emptyPBlock :: NS -> PBlock
emptyPBlock = PBlock emptyBlock [] Map.empty

pnodes :: PBlock -> Map Id PNode
pnodes (PBlock _ refs _ ns) = pnodes' ns 0 $ reverse refs
pnodes' :: NS -> Level -> [PNode] -> Map Id PNode
pnodes' ns d = Map.fromList . zip (Volatile ns d <$> [0..])

latentPNodes :: PBlock -> Map Id PNode
latentPNodes pb@(PBlock _ _ given _) =
  pnodes pb Map.\\ Map.fromList [(k,v) | (LVar k,v) <- Map.toList given]

showPNodes :: NS -> Level -> [PNode] -> String -> String
showPNodes ns d refs ret = "do "++ indent' 0 3 s ++"\n"++
                           "   return "++ ret
  where s = unlines [show i ++" <- "++ show r | (i,r) <- Map.toList $ pnodes' ns d refs]

showPBlock :: PBlock -> String -> String
showPBlock (PBlock (Block block) refs _ ns) = showBlock block . showPNodes ns 0 (reverse refs)

-- lift into Block
liftExprBlock :: MonadState PBlock m => State Block b -> m b
liftExprBlock s = do
    PBlock block rhs given ns <- get
    let (ret, block') = runState s block
    put $ PBlock block' rhs given ns
    return ret

-- | Stochaskell probabilistic program intermediate representation
newtype P t = P { fromProg :: State PBlock t }
  deriving (Functor,Applicative,Monad)
type Prog t = P t
instance (Eq t) => Eq (P t) where p == q = runProg "eq" p == runProg "eq" q
runProg :: NS -> P a -> (a, PBlock)
runProg ns p = runState (fromProg p) $ emptyPBlock ns

instance (ExprTuple t) => Show (P t) where
  show p = showPBlock pb $ show rets
    where (rets, pb) = runProgExprs "show" p
instance (ExprTuple a, ExprTuple b) => Show (a -> P b) where
  show p = "\\input ->\n"++ (indent . showPBlock pb $ show rets)
    where (rets, pb) = runProgExprs "show" . p . entuple $ symbol "input"

fromProgExprs :: (ExprTuple t) => P t -> State PBlock [NodeRef]
fromProgExprs p = do
  es <- fromExprTuple <$> fromProg p
  mapM (liftExprBlock . fromDExpr) es

runProgExprs :: (ExprTuple t) => NS -> P t -> ([NodeRef], PBlock)
runProgExprs ns p = runState (fromProgExprs p) $ emptyPBlock ns

-- all samples whose density depends on the value of non-fixed parameters
-- ie. not constant wrt the given data
modelSkeleton :: PBlock -> Set Id
modelSkeleton pb@(PBlock block _ given _) = tparams
  where samples = pnodes pb
        params = Map.keysSet samples Set.\\
          (Set.fromList . mapMaybe getId' . Set.toList $ Map.keysSet given)
        dependents xs = Set.union xs . Map.keysSet $
          Map.filter (not . Set.null . Set.intersection xs . dependsPNode block) samples
        dparams = fixpt dependents params
        tparams = Set.foldr Set.union dparams $ Set.map g dparams
        -- TODO: warn when any samples\\tparams have zero density
          where g i = let n = fromJust $ Map.lookup i samples
                      in Set.filter isInternal $ dependsPNode block n

evalPBlock :: (ExprTuple t) => PBlock -> [NodeRef] -> Env -> Either String t
evalPBlock (PBlock block _ given _) rets env = do
  xs <- sequence (evalNodeRef (Map.union (Map.map fst given) env) block <$> rets)
  return $ fromConstVals xs

runLambdaP :: NS -> [(Id,Type)] -> State PBlock r -> State Block (Lambda r, [PNode])
runLambdaP ns ids s = do
  block <- get
  let d = nextLevel block
      (ret, PBlock (Block (dag:block')) acts _ _) = runState s $
        PBlock (deriveBlock (DAG d ids Bimap.empty) block) [] Map.empty ns
  put $ Block block'
  return (Lambda dag ret, reverse acts)

caseP :: Int -> DExpr -> [[DExpr] -> P [DExpr]] -> P [DExpr]
caseP n e ps = P $ do
  k <- liftExprBlock $ fromDExpr e
  case k of
    Data c args _ -> do
      block <- liftExprBlock get
      let p = ps !! c
          args' = reDExpr emptyEEnv block <$> args
      fromProg $ p args'
    _ -> fromProg $ caseP' n k ps

caseP' :: Int -> NodeRef -> [[DExpr] -> P [DExpr]] -> P [DExpr]
caseP' n k ps = distDs n $ \ns -> do
  d <- getNextLevel
  let tss = case typeRef k of
        UnionT t -> t
        IntT -> repeat []
  cases <- sequence $ do
    (ts,p) <- zip tss ps
    let ids = [(Dummy d i, t) | (i,t) <- zip [0..] ts]
        args = [DExpr . return $ Var i t | (i,t) <- ids]
    return . runLambdaP ns ids $ do
      r <- fromProg $ p args
      liftExprBlock . sequence $ fromDExpr <$> r
  return $ Switch k cases ns (tupleT . foldr1 (zipWith E.coerce) $ map typeRef . fHead . fst <$> cases)

fromCaseP :: forall c t. (Constructor c, ExprTuple t) => (c -> P t) -> Expression c -> P t
fromCaseP p e = toExprTuple <$> caseP n (erase e)
  [fmap fromExprTuple . p . construct Expression c | c <- cs]
  where Tags cs = tags :: Tags c
        TupleSize n = tupleSize :: TupleSize t

switchOf :: (Constructor c, ExprTuple t) => (Expression c -> P t) -> Expression c -> P t
switchOf f = fromCaseP (f . fromConcrete)

-- | iterate a Markov chain over the given integer range, eg.
--
-- > chainRange' (1,10) (\i x -> normal x 1) 0 -- 10-step standard normal random walk from 0
chainRange' :: forall t. (ExprTuple t) => Interval Z -> (Z -> t -> P t) -> t -> P t
chainRange' (lo,hi) p x = fmap entuple . dist' $ \ns -> do
  let TypeIs t = typeOf :: TypeOf t
      ids = [(Dummy 99 0, IntT), (Dummy 99 1, t)]
      args = [DExpr . return $ Var i t | (i,t) <- ids]
  (lam, racts) <- runLambdaP ns ids $ do
    r <- fromProg $ p (Expression $ head args) (entuple . Expression $ args !! 1)
    liftExprBlock . fromExpr $ detuple r
  lo' <- fromExpr lo
  hi' <- fromExpr hi
  x' <- fromExpr $ detuple x
  return $ Chain (lo',hi') racts lam x' ns t

-- | convenience function for calling 'chainRange'' starting at 1 and ignoring indices
chain' :: (ExprTuple t) => Z -> (t -> P t) -> t -> P t
chain' n p = chainRange' (1,n) (const p)

unfoldP :: forall t f.
  (Traversable f, Constructor (f t), ExprTuple t, ExprType (f (FixE f)), ExprType (f t))
  => (t -> P (Expression (f t))) -> t -> P (FixE' f)
unfoldP f r = dist' $ \ns -> do
  seed <- fromExpr $ detuple r
  block <- get
  d <- gets nextLevel
  let i = Dummy 99 0
      s = typeRef seed
      g :: t -> P (FixE f)
      g x = fmap FixE . dist $ do
        i <- fromExpr (detuple x)
        return $ Dist "unfold" [i] RecursiveT
      q :: P (Expression (f (FixE f)))
      q = do
        y <- f (entuple . expr . return $ Var i s)
        fromCaseP (fmap fromConcrete . traverse g) y
      TypeIs t = typeOf :: TypeOf (Expression (f (FixE f)))
  (lam, racts) <- runLambdaP ns [(i,s)] $ do
    z <- fromProg q
    liftExprBlock $ fromExpr z
  return $ UnfoldP racts lam seed ns t

-- | creates a mixture distribution, eg.
--
-- > mixture [(0.7, uniform 0 1), (0.3, normal 0 1)]
mixture :: forall t. (ExprTuple t) => [(R, P t)] -> P t
mixture qps = do
  k <- pmf qv :: P Z
  toExprTuple <$> caseP n (erase k) (const . fmap fromExprTuple <$> progs)
  where (qs,progs) = unzip qps
        qv = blockVector [cast q | q <- qs] :: RVec
        TupleSize n = tupleSize :: TupleSize t

-- | like 'mixture', but produces a flattened intermediate representation
-- that uses a case expression rather than a switch statement
mixture' :: (ExprTuple t) => [(R, P t)] -> P t
mixture' qps = do
  k <- pmf qv
  rs <- sequence progs
  return $ caseZ k rs
  where (qs,progs) = unzip qps
        qv = blockVector [cast q | q <- qs] :: RVec

conditionGuards :: B -> P t -> P t
conditionGuards c p = P $ do
  r <- fromProg p
  PBlock block dists given ns <- get
  let given' = Map.mapKeys (\k -> LCond True k $ erase c) given
  put $ PBlock block dists given' ns
  return r

type instance BooleanOf (P t) = B
instance (IfB t, BooleanOf t ~ B) => IfB (P t) where
  ifB c p q = do
    x <- conditionGuards c p
    y <- conditionGuards (notB c) q
    return $ ifB c x y

initEnv :: (ExprTuple t) => NS -> P t -> Maybe t -> Env
initEnv ns prog = \case
  Just init -> unifyTuple block rets init given
  Nothing -> Map.map fst given
  where (rets, pb@(PBlock block _ given _)) = runProgExprs ns prog


------------------------------------------------------------------------------
-- PRIMITIVE DISTRIBUTIONS                                                  --
------------------------------------------------------------------------------

dist :: State Block PNode -> P (Expression t)
dist = dist' . const
dist' :: (NS -> State Block PNode) -> P (Expression t)
dist' s = Expression <$> distD s
distD :: (NS -> State Block PNode) -> P DExpr
distD s = P $ do
    PBlock _ _ _ ns <- get
    d <- liftExprBlock $ s ns
    PBlock block rhs given _ <- get
    put $ PBlock block (d:rhs) given ns
    let depth = dagLevel $ topDAG block
        k = length rhs
        name = Volatile ns depth k
        t = typePNode d
        v = Var name t
    _ <- liftExprBlock . simplify $ Apply "getExternal" [v] t
    return (DExpr $ return v)
distDs :: Int -> (NS -> State Block PNode) -> P [DExpr]
distDs n s = P $ do
  e <- fromProg $ distD s
  return $ if n == 1 then [e] else extractD e 0 <$> [0..(n-1)]

-- | truncate distribution to given bounds
truncated :: Expression t -> Expression t -> P (Expression t) -> P (Expression t)
truncated a b p = P $ do
  rollback <- get
  i <- liftExprBlock $ fromExpr a
  j <- liftExprBlock $ fromExpr b
  x <- fromProg p
  v <- liftExprBlock $ fromExpr x
  pb <- get
  let Var name t = v
      PBlock block (d:rhs) given ns = pb
  if name /= Volatile ns (dagLevel $ topDAG block) (length rhs) then
    trace "truncated: program does not appear to be primitive, falling back to guard" $ do
      put rollback
      fromProg $ truncated' a b p
  else do
    let g k | (Const c _) <- k, isInfinite c = Nothing
            | otherwise = Just k
        t' = SubrangeT t (g i) (g j)
        d' = d { typePNode = t' }
        Block (dag:par) = block
        Just ptr = Apply "getExternal" [Var name t] t `Bimap.lookup` bimap dag
        dag' = dag {bimap =
          Apply "getExternal" [Var name t'] t' `Bimap.insert` ptr $ bimap dag}
        block' = Block (dag':par)
    put $ PBlock block' (d':rhs) given ns
    return (expr $ return (Var name t'))

truncated' :: Expression t -> Expression t -> P (Expression t) -> P (Expression t)
truncated' a b p = do
  x <- p
  guard $ a <=* x &&* x <=* b
  return x

{-
transform :: (ExprTuple t) => P t -> P t
transform prog = P $ do
  PBlock block acts given ns <- get
  assert (given == emptyEnv) $ return ()
  let d = nextLevel block
      dBlock = deriveBlock (DAG d [] Bimap.empty) block
      (rets, PBlock dBlock'@(Block (dag:block')) acts' _ _) =
        runState (fromProgExprs prog) $ PBlock dBlock [] emptyEnv ns
      ids = Dummy (d-1) <$> [0..(length rets - 1)]
      zs = zipWith Var ids $ map typeRef rets
      eenv = solveTupleD dBlock' rets (DExpr . return <$> zs) emptyEEnv
      invfs = [fromMaybe (error "not invertible") $ lookupEEnv x eenv
              | (x,t) <- map (Volatile ns d) [0..] `zip` map typePNode (reverse acts')]
      ts = typeRef <$> rets
      t = if length rets > 1 then TupleT ts else head ts
      jacobian = [[collapseArray $ derivD emptyEEnv u (Var (Dummy 0 j) (ts!!j))
                  | u <- invfs] | j <- [0..(length rets - 1)]]
      pnode = ITDist dag acts' rets (reverse invfs) jacobian t
  put $ PBlock (Block block') (pnode:acts) emptyEnv ns
  let k = length acts
      name = Volatile ns (d-1) k
      v = Var name t
  return $ entuple (expr $ return v)
-}

instance Distribution Bernoulli R P B where
    sample (Bernoulli p) = dist $ do
        i <- fromExpr p
        return $ Dist "bernoulli" [i] boolT
    sample (BernoulliLogit l) = dist $ do
        i <- fromExpr l
        return $ Dist "bernoulliLogit" [i] boolT

instance Distribution Bernoullis RVec P BVec where
    sample (Bernoullis p) = dist $ do
        i <- fromExpr p
        let (ArrayT _ [n] _) = typeRef i
        return $ Dist "bernoullis" [i] (ArrayT Nothing [n] boolT)
    sample (BernoulliLogits l) = dist $ do
        i <- fromExpr l
        let (ArrayT _ [n] _) = typeRef i
        return $ Dist "bernoulliLogits" [i] (ArrayT Nothing [n] boolT)

instance Distribution Beta (R,R) P R where
    sample (Beta (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "beta" [i,j] RealT

instance Distribution Cauchy (R,R) P R where
    sample (Cauchy (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "cauchy" [i,j] RealT

instance Distribution Cauchys (RVec,RVec) P RVec where
    sample (Cauchys (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "cauchys" [i,j] (typeRef i)

instance Distribution Gamma (R,R) P R where
    sample (Gamma (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "gamma" [i,j] RealT

instance Distribution InvGamma (R,R) P R where
    sample (InvGamma (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "inv_gamma" [i,j] RealT

instance Distribution Geometric R P Z where
    sample (Geometric p) = dist $ do
        i <- fromExpr p
        return $ Dist "geometric" [i] IntT

instance Distribution LKJ (R, Interval Z) P RMat where
    sample (LKJ (v,(a,b))) = dist $ do
        i <- fromExpr v
        l <- fromExpr a
        h <- fromExpr b
        return $ Dist "lkj_corr" [i] (ArrayT (Just "corr_matrix") [(l,h),(l,h)] RealT)

instance Distribution Logistic (R,R) P R where
    sample (Logistic (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "logistic" [i,j] RealT

instance Distribution NegBinomial (R,R) P Z where
    sample (NegBinomial (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "neg_binomial" [i,j] IntT

instance Distribution Normal (R,R) P R where
    sample (Normal (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normal" [i,j] RealT

instance Distribution Normals (RVec,RVec) P RVec where
    sample (Normals (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normals" [i,j] (typeRef i)

instance Distribution Normals (RMat,RMat) P RMat where
    sample (Normals (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normals" [i,j] (typeRef i)

instance Distribution Normal (RVec,RMat) P RVec where
    sample (Normal (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "multi_normal" [i,j] (typeRef i)

instance (ExprType t) => Distribution OrderedSample (Z, P (Expression t)) P (Expression [t]) where
    sample (OrderedSample (n,prog)) = P $ do
        i <- liftExprBlock $ fromExpr n
        PBlock block rhs given ns <- get
        let (_, PBlock block' [act] _ _) =
              runState (head <$> fromProgExprs prog) $ PBlock block [] Map.empty ns
            d = HODist "orderedSample" act [i] (ArrayT Nothing [(Const 1 IntT,i)] (typePNode act))
        put $ PBlock block' (d:rhs) given ns
        let depth = dagLevel $ topDAG block
            k = length rhs
            name = Volatile ns depth k
            t = typePNode d
            v = Var name t
        _ <- liftExprBlock . simplify $ Apply "getExternal" [v] t
        return (expr $ return v)

instance Distribution PMF RVec P Z where
    sample (PMF probs) = dist $ do
        l <- fromExpr probs
        return $ Dist "pmf" [l] IntT

instance Distribution Poisson R P Z where
    sample (Poisson a) = dist $ do
        i <- fromExpr a
        return $ Dist "poisson" [i] IntT

instance Distribution PoissonProcess (R, R -> R, R) P RVec where
    sample (PoissonProcess (t, rate, mean)) = dist $ do
        i <- fromExpr t
        d <- getNextLevel
        lam <- runLambda [(Dummy d 1, RealT)] . fromExpr . rate . expr . return $
          Var (Dummy d 1) RealT
        j <- simplify $ Function lam RealT
        k <- fromExpr mean
        return . Dist "poissonProcess" [i,j,k] $
          ArrayT (Just "vector") [(Const 1 IntT, Unconstrained IntT)] RealT

instance Distribution Uniform (R,R) P R where
    sample (Uniform (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniform" [i,j] RealT

instance Distribution Uniforms (RVec,RVec) P RVec where
    sample (Uniforms (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniforms" [i,j] (typeRef i)

instance Distribution Uniforms (RMat,RMat) P RMat where
    sample (Uniforms (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniforms" [i,j] (typeRef i)

instance Distribution Uniform (Z,Z) P Z where
    sample (Uniform (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "discreteUniform" [i,j] IntT

instance Distribution Wishart (R,RMat) P RMat where
    sample (Wishart (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "wishart" [i,j] (typeRef j)

instance Distribution InvWishart (R,RMat) P RMat where
    sample (InvWishart (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        let (ArrayT _ sh t) = typeRef j
        return $ Dist "inv_wishart" [i,j] (ArrayT (Just "cov_matrix") sh t)

-- | multivariate normal via Cholesky decomposition of covariance matrix
normalChol :: Z -> RVec -> RMat -> P RVec
normalChol n mu cov = do
  --w <- joint vector [ normal 0 1 | _ <- 1...n ]
  w <- normals (vector [ 0 | _ <- 1...n ])
               (vector [ 1 | _ <- 1...n ])
  return (mu + chol cov #> w)

-- | iid 'normalChol' samples as rows
normalsChol :: Z -> Z -> RVec -> RMat -> P RMat
normalsChol n k mu cov = do
  --w <- joint vector [ normal 0 1 | i <- 1...n, j <- 1...k ]
  w <- normals (matrix [ 0 | i <- 1...n, j <- 1...k ])
               (matrix [ 1 | i <- 1...n, j <- 1...k ])
  return $ asRow mu + (w <> tr' (chol cov))

-- | Gaussian process conditioned on observations
normalCond :: Z -> (Expression t -> Expression t -> R) -> R -> Expression [t] -> RVec -> Expression t -> P R
normalCond n cov noise s y x = normal m (sqrt v)
  where c = matrix [ cov (s!i) (s!j) + ifB (i ==* j) noise 0
                   | i <- 1...n, j <- 1...n ] :: RMat
        k = vector [ cov (s!i) x | i <- 1...n ] :: RVec
        m = y <.> (c <\> k)
        v = cov x x + noise - k <.> (c <\> k)


------------------------------------------------------------------------------
-- LOOPS                                                                    --
------------------------------------------------------------------------------

instance forall r f. (ExprType r, ExprType f, Show f) =>
         Joint P Z (Expression r) (Expression f) where
  joint _ ar = P $ do
    let shp = sequence . flip map (shape ar) $ \(a,b) -> do
          i <- fromExpr a
          j <- fromExpr b
          return (i,j)
    loopShape <- liftExprBlock shp
    PBlock block dists given ns <- get
    let d = nextLevel block
        ids = [(Dummy d i, IntT) | i <- [1..length loopShape]]
        p = ar ! [expr . return $ Var i t | (i,t) <- ids]
        (ret, PBlock (Block (dag:block')) [act] _ _) =
          runState (head <$> fromProgExprs p) $
            PBlock (deriveBlock (DAG d ids Bimap.empty) block) [] Map.empty ns
        TypeIs t = typeOf :: TypeOf r -- TODO: incorrect type for transformed case
        loopType = do
          sh <- shp
          case t of
            ArrayT{} -> do
              let ([eret], PBlock eblock _ _ _) = runProgExprs "joint" (ar!1)
              etype <- extractType emptyEEnv eblock (typeRef eret)
              let ArrayT _ sh' t = etype
              return $ ArrayT Nothing (sh ++ sh') t
            _ -> return $ ArrayT Nothing sh t
    put $ PBlock (Block block') dists given ns
    loop <- Loop loopShape (Lambda dag act) <$> liftExprBlock loopType
    PBlock block dists given ns <- get
    put $ PBlock block (loop:dists) given ns
    let name = Volatile ns (d-1) (length dists)
    _ <- liftExprBlock . simplify $
      Apply "getExternal" [Var name (typePNode loop)] (typePNode loop)
    return $ case ret of
      Var (Volatile ns depth 0) _ | depth == d ->
        expr $ Var name <$> loopType :: Expression f
      Index vec [Var (Volatile ns depth 0) _] | depth == d ->
        expr $ do
          sh <- shp
          v <- Var name <$> loopType
          let ref = Index v (reverse [Var i t | (i,t) <- ids])
          floatNode $ Array sh (Lambda dag (Index vec [ref])) (typeRef v)
      _ -> error $ "non-trivial transform in joint: "++ show ret


------------------------------------------------------------------------------
-- CONDITIONING                                                             --
------------------------------------------------------------------------------

type instance ConditionOf (P ()) = Expression Bool
instance MonadGuard P where
    guard = P . go where
      go cond = do
        v <- liftExprBlock (fromExpr cond)
        let Var (Internal 0 i) _ = v
        PBlock block dists given ns <- get
        let dag = topDAG block
        assert (i == length (nodes dag) - 1) $ return ()
        case lookup i (nodes dag) of
          Just (Apply "==" [Var j t, Const a _] _) ->
            let dag' = dag { bimap = Bimap.deleteR i (bimap dag) }
            in put $ PBlock (deriveBlock dag' block) dists
                     (Map.insert (LVar j) (a,t) given) ns
          _ -> go (cond ==* true)

dirac :: Expression t -> P (Expression t)
dirac c = do
  x <- dist $ do
    i <- fromExpr c
    return $ Dist "dirac" [i] (typeRef i)
  guard $ x ==* c
  return x


------------------------------------------------------------------------------
-- PROBABILITY DENSITIES                                                    --
------------------------------------------------------------------------------

-- | 'solve' applied to program outputs
solveP :: (ExprTuple t) => P t -> t -> EEnv
solveP prog vals = solveP' "solve" prog vals emptyEEnv

solveP' :: (ExprTuple t) => String -> P t -> t -> EEnv -> EEnv
solveP' ns prog = solveTuple block rets
  where (rets, pb) = runProgExprs ns prog
        block = definitions pb

pdf :: (ExprTuple t, Show t) => P t -> t -> R
pdf p = exp . lpdf p

-- | compute log pdf of distribution represented by probabilistic program
lpdf :: (ExprTuple t, Show t) => P t -> t -> R
lpdf prog vals = subst (substEEnv env') $ pdfPBlock True env' pb - adjust
  where (rets, pb@(PBlock block acts _ ns)) = runProgExprs "lpdf" prog
        dummy = constSymbolLike "lpdf_dummy_input" vals
        EEnv env = solveTuple block rets dummy emptyEEnv
        env' = EEnv $ Map.insert (LVar $ Symbol "lpdf_dummy_input" True) (erase $ detuple vals)
                    $ Map.filterWithKey p env
        p (LVar (Volatile "lpdf" _ _)) _ = True
        p LVar{} _ = False
        p _ _ = True
        jacobian =
          [ [ derivNodeRef env' block r (Var (Volatile ns 0 i) (typePNode d))
            | (i,d) <- zip [0..] (reverse acts), not (isDiscrete (typePNode d)) ]
          | r <- rets, not (isDiscrete (typeRef r)) ]
        isLowerTri = and [ isZ `all` drop i row | (i,row) <- zip [1..] jacobian ]
        diagonal = [ row !! i | (i,row) <- zip [0..] jacobian ]
        adjust | isLowerTri = Expression $ sum (logDet <$> diagonal)
               | otherwise = trace "WARN: jacobian is not block triangular" $
                    Expression $ logDet (blockMatrix jacobian)
        isZ e = case ret of
          Const c _ -> isZeros c
          Var i@Internal{} _ | Apply "zeros" _ _ <- lookupBlock i block -> True
          _ -> False
          where (ret,block) = runDExpr e

-- | compute joint pdf of primitive random variables within the given program
lpdfAux :: (ExprTuple t, Show t) => P t -> t -> R
lpdfAux prog vals = subst (substEEnv env') $ pdfPBlock True env' pb
  where (rets, pb) = runProgExprs "lpdfAux" prog
        dummy = constSymbolLike "lpdfAux_dummy_input" vals
        EEnv env = solveTuple (definitions pb) rets dummy emptyEEnv
        env' = EEnv $ Map.insert (LVar $ Symbol "lpdfAux_dummy_input" True) (erase $ detuple vals)
                    $ Map.filterWithKey p env
        p (LVar (Volatile "lpdfAux" _ _)) _ = True
        p LVar{} _ = False
        p _ _ = True

lpdfAuxC :: (Constructor t, Show t) => P (Expression t) -> Expression t -> R
lpdfAuxC = caseOf . lpdfAux

lpdfCond :: (ExprTuple s, ExprTuple t, Show t) => (s -> P t) -> s -> t -> R
lpdfCond p v x = subst (substEEnv env) $ (lpdf . p) dummy x
  where dummy = constSymbolLike "lpdfCond_dummy_input" v
        env = EEnv $ Map.singleton (LVar $ Symbol "lpdfCond_dummy_input" True) (erase $ detuple v)

pdfPBlock :: Bool -> EEnv -> PBlock -> R
pdfPBlock lg env pb@(PBlock block refs _ ns) = pdfPNodes (pnodes pb) ns lg env block refs

pdfPNodes :: Map Id PNode -> NS -> Bool -> EEnv -> Block -> [PNode] -> R
pdfPNodes r ns lg env block refs = (if lg then sum else product) $ do
    (i,d) <- zip [0..] $ reverse refs
    let ident = Volatile ns (dagLevel $ topDAG block) i
    return $ case lookupEEnv ident env of
      Just val -> pdfPNode r lg env block d val
      Nothing  -> trace (show ident ++" is unconstrained") $ if lg then 0 else 1

pdfPNode :: Map Id PNode -> Bool -> EEnv -> Block -> PNode -> DExpr -> R
pdfPNode _ lg env block (Dist f args _) x = expr $ do
  i <- fromDExpr x
  case i of
    Unconstrained _ -> fromExpr $ if lg then 0 else 1 :: R
    PartiallyConstrained{} -> error "TODO: partially constrained"
    _ -> do
      js <- sequence $ extractNodeRef simplifyNodeRef simplify env block <$> args
      simplify $ Apply (f ++ if lg then "_lpdf" else "_pdf") (i:js) RealT
pdfPNode r lg env block (Loop _ lam _) a
  | (Unconstrained _,_) <- runDExpr a = if lg then 0 else 1
  | (PartiallyConstrained{},_) <- runDExpr a = error "TODO: partially constrained"
  | lg        = foldlE (\l e -> l + f e) 0 idxs
  | otherwise = foldlE (\p e -> p * f e) 1 idxs
  where n = Expression $ vectorSize a
        idxs = vector (1...n) :: ZVec
        f e = let j = erase e in pdfJoint r lg env block lam [j] (a!j)
pdfPNode r lg env block (HODist "orderedSample" d [n] _) x = expr $ do
  i <- fromDExpr x
  case i of
    Unconstrained _ -> fromExpr $ if lg then 0 else 1 :: R
    PartiallyConstrained [(lo,hi)] [(id,t)] [([k],v)] _ -> fromExpr $
      pdfOrderStats r lg env block d n (lo,hi) (id,t) (k,v)
    _ | lg -> fromExpr $ foldlE g (logFactorial' n') (Expression x)
  where n' = Expression $ reDExpr env block n
        g l z = l + pdfPNode r lg env block d (erase z)
pdfPNode _ lg env block (Switch hd alts ns _) x = Expression $
  caseD hd' [const [erase e] | e <- alts']
  where hd' = reDExpr env block hd
        p (LVar (Volatile ns' _ _)) _ | ns' == ns = True
        p _ _ = False
        alts' = do
          (Lambda ldag rets, pns) <- alts
          let block' = deriveBlock ldag block
              EEnv env' = solveTupleD block' rets (entupleD (length rets) x) emptyEEnv
          return . subst (substEEnv . EEnv $ Map.filterWithKey p env') $
            pdfPNodes undefined ns lg (env `unionEEnv` EEnv env') block' pns
pdfPNode _ _ _ _ node x = error $ "pdfPNode "++ show node ++" "++ show x

pdfJoint :: Map Id PNode -> Bool -> EEnv -> Block -> Lambda PNode -> [DExpr] -> DExpr -> R
pdfJoint r lg env block (Lambda body pn) js = pdfPNode r lg env' block' pn
  where block' = deriveBlock body block
        env' = bindInputs body js `unionEEnv` env

pdfOrderStats :: Map Id PNode -> Bool -> EEnv -> Block -> PNode -> NodeRef
              -> Interval DExpr -> (Id,Type) -> (DExpr,DExpr) -> R
pdfOrderStats r lg env block d n (lo,hi) (dummy,dummyT) (k,v) =
  ifB (Expression lo >* Expression hi) (if lg then 0 else 1) $ if lg
  then    logFactorial' n'  + intervalL +     sum' intervals + intervalR +     sum' points
  else cast (factorial' n') * intervalL * product' intervals * intervalR * product' points
  where fcdf = cdfPNode env block d
        n' = reExpr env block n :: Z
        kv' i = let envi = EEnv $ Map.singleton (LVar dummy) i
                in (Expression $ substD envi k, substD envi v) :: (Z,DExpr)
        intervalL = let (i,x) = kv' lo in if lg
          then log (fcdf x) *  cast (i-1) -    logFactorial' (i-1)
          else     fcdf x ** cast (i-1) / cast (factorial' (i-1))
        interval (i,x) (j,y) = if lg
          then log (fcdf y - fcdf x) *  cast (j-i-1) -    logFactorial' (j-i-1)
          else     (fcdf y - fcdf x) ** cast (j-i-1) / cast (factorial' (j-i-1))
        intervals = vector [ interval (kv' $ erase i) (kv' . erase $ i+1)
                           | i::Z <- Expression lo...(Expression $ hi-1) ] :: RVec
        intervalR = let (j,y) = kv' hi in if lg
          then log (1 - fcdf y) *  cast (n'-j) -    logFactorial' (n'-j)
          else     (1 - fcdf y) ** cast (n'-j) / cast (factorial' (n'-j))
        points = vector [ let (_,x) = (kv' $ erase i) in pdfPNode r lg env block d x
                        | i::Z <- Expression lo...Expression hi ] :: RVec
        sum' = foldlE (+) 0
        product' = foldlE (*) 1

cdfPNode :: EEnv -> Block -> PNode -> DExpr -> R
cdfPNode env block (Dist f args _) x = expr $ do
  i <- fromDExpr x
  js <- sequence $ extractNodeRef simplifyNodeRef simplify env block <$> args
  simplify $ Apply (f ++"_cdf") (i:js) RealT

-- TODO: deprecate in favour of pdf
density :: (ExprTuple t) => P t -> t -> LF.LogFloat
density prog vals = densityPBlock env' pb / adjust
  where (rets, pb@(PBlock block acts _ ns)) = runProgExprs "density" prog
        env = unifyTuple block rets vals Map.empty
        env' = evalBlock block env
        jacobian = [ [ diffNodeRef env' block r (Volatile ns 0 i) (typePNode d)
                     | (i,d) <- zip [0..] (reverse acts), not (isDiscrete (typePNode d)) ]
                   | r <- rets, not (isDiscrete (typeRef r)) ]
        upperTri = [ drop i row | (i,row) <- zip [1..] jacobian ]
        diagonal = [ row !! i | (i,row) <- zip [0..] jacobian ]
        ldet = LF.logToLogFloat . real . logDet :: ConstVal -> LF.LogFloat
        adjust | and (all isZeros <$> upperTri) = product (map ldet diagonal)
               | otherwise = error $ "jacobian is not block triangular: "++ show upperTri

density' :: (ExprTuple t) => P t -> t -> LF.LogFloat
density' prog vals = densityPBlock env' pb
  where (rets, pb) = runProgExprs "density'" prog
        env = unifyTuple (definitions pb) rets vals Map.empty
        env' = evalBlock (definitions pb) env

densityPBlock :: Env -> PBlock -> LF.LogFloat
densityPBlock env (PBlock block refs _ ns) = product $!!! do
    (i,d) <- zip [0..] $ reverse refs
    let ident = Volatile ns (dagLevel $ topDAG block) i
    return $ case Map.lookup (LVar ident) env of
      Just val -> densityPNode env block d val
      Nothing  -> trace (show ident ++" is unconstrained") $ LF.logFloat 1

densityPNode :: Env -> Block -> PNode -> ConstVal -> LF.LogFloat
densityPNode env block (Dist "bernoulli" [p] _) x =
    LF.logFloat (if toBool x then p' else 1 - p')
  where p' = toDouble . fromRight' $ evalNodeRef env block p
densityPNode env block (Dist "bernoulliLogit" [l] _) a
    | x == 1 = LF.logFloat p
    | x == 0 = LF.logFloat (1 - p)
    | otherwise = LF.logFloat 0
  where x = toRational a
        l' = toDouble . fromRight' $!! evalNodeRef env block l
        p = 1 / (1 + exp (-l'))
densityPNode env block (Dist "pmf" [q] _) x = LF.logFloat p
  where q' = map toDouble . toList . fromRight' $!! evalNodeRef env block q
        p = fromMaybe 0 . lookup x $!! zip [1..] q'
densityPNode env block (Dist "gamma" [a,b] _) x
    | x' >= 0 = LF.logToLogFloat $ lpdfGamma x' a' b'
    | otherwise = LF.logFloat 0
  where a' = toDouble . fromRight' $!! evalNodeRef env block a
        b' = toDouble . fromRight' $!! evalNodeRef env block b
        x' = toDouble x
densityPNode env block (Dist "inv_gamma" [a,b] _) x
    | x' >= 0 = LF.logToLogFloat l
    | otherwise = LF.logFloat 0
  where a' = toDouble . fromRight' $!! evalNodeRef env block a
        b' = toDouble . fromRight' $!! evalNodeRef env block b
        x' = toDouble x
        l = a' * log b' - (a' + 1) * log x' - b' / x' - logGamma a'
densityPNode env block (Dist "geometric" [t] _) x = p * q^k
  where t' = toDouble . fromRight' $!! evalNodeRef env block t
        p = LF.logFloat t'
        q = LF.logFloat (1 - t')
        k = toInteger x
densityPNode env block (Dist "normal" [m,s] _) x =
    LF.logToLogFloat $ logPdf (Rand.Normal m' s') (toDouble x)
  where m' = toDouble . fromRight' $!! evalNodeRef env block m
        s' = toDouble . fromRight' $!! evalNodeRef env block s
densityPNode env block (Dist "normals" [m,s] _) x = product
    [LF.logToLogFloat $ logPdf (Rand.Normal a b) c | (a,b,c) <- zip3 m' s' x']
  where m' = map toDouble . toList . fromRight' $!! evalNodeRef env block m
        s' = map toDouble . toList . fromRight' $!! evalNodeRef env block s
        x' = map toDouble $ toList x
densityPNode env block (Dist "multi_normal" [m,s] _) x =
    LF.logToLogFloat $ -0.5 * real ((x' <.> (s' <\> x')) + logDet s' + n * log (2*pi))
  where m' = fromRight' $!! evalNodeRef env block m
        s' = fromRight' $!! evalNodeRef env block s
        n = integer $ length (toList m')
        x' = x - m'
densityPNode env block (Dist "poisson" [l] _) x =
    LF.logToLogFloat $ lpdfPoisson k l'
  where l' = toDouble . fromRight' $!! evalNodeRef env block l
        k = toInteger x
densityPNode env block (Dist "uniform" [a,b] _) x =
    LF.logFloat $ if a' <= x' && x' <= b' then 1/(b' - a') else 0
  where a' = toDouble . fromRight' $!! evalNodeRef env block a
        b' = toDouble . fromRight' $!! evalNodeRef env block b
        x' = toDouble x
densityPNode env block (Dist "uniforms" [as,bs] _) xs = product
    [LF.logFloat $ if a <= x && x <= b then 1/(b - a) else 0 | (a,b,x) <- zip3 as' bs' xs']
  where as' = map toDouble . toList . fromRight' $!! evalNodeRef env block as
        bs' = map toDouble . toList . fromRight' $!! evalNodeRef env block bs
        xs' = map toDouble $ toList xs
densityPNode env block (Dist "discreteUniform" [a,b] _) x =
    LF.logFloat $ if a' <= x' && x' <= b' then 1/fromInteger (b' - a' + 1) else 0
  where a' = toInteger . fromRight' $!! evalNodeRef env block a
        b' = toInteger . fromRight' $!! evalNodeRef env block b
        x' = toInteger x
densityPNode _ _ (Dist d _ _) _ = error $ "unrecognised density "++ d

densityPNode env block (Loop shp (Lambda ldag body) _) a = product
    [ let env' = Map.fromList (inputsL ldag `zip` i) `Map.union` env
      in densityPNode env' block' body (fromRational x)
    | (i,x) <- evalRange env block shp `zip` entries a ]
  where block' = deriveBlock ldag block

densityPNode env block (HODist "orderedSample" d [n] _) a = lfact n' * product
    [ densityPNode env block d (fromRational x) | x <- entries a ]
  where n' = toInteger . fromRight' $!! evalNodeRef env block n


------------------------------------------------------------------------------
-- SAMPLING                                                                 --
------------------------------------------------------------------------------

-- | like 'runStep' for programs that don't take parameters
simulate :: (ExprTuple t) => P t -> IO t
simulate = sampleP

sampleP :: (ExprTuple t) => P t -> IO t
sampleP = sampleP' emptyEnv
sampleP' :: (ExprTuple t) => Env -> P t -> IO t
sampleP' env p = fromConstVals <$> samplePBlock env pb rets
  where (rets, pb) = runProgExprs "sim" p

samplePBlock :: Env -> PBlock -> [NodeRef] -> IO [ConstVal]
samplePBlock env (PBlock block refs given ns) rets = go where
  go = do
    env' <- samplePNodes (evalBlock block env, block, Nothing) idents
    let check (k@LVar{},(v,t)) = case Map.lookup k env' of
          Nothing -> error $ "couldn't find "++ show k ++" in "++ show env'
          Just c -> v == c
        check (LCond _ k c,(v,t)) =
          let c' = fromRight' $ evalD env' c
          in c' == 0 || check (k,(v,t))
        env'' = Map.filterWithKey (const . p' . getId') env'
    if check `all` Map.toList given
    then return $ map (fromRight' . evalNodeRef env'' block) rets
    else {-trace "rejecting sample due to failed conditions"-} go
  idents = [ (Volatile ns (dagLevel $ topDAG block) i, d)
           | (i,d) <- zip [0..] $ reverse refs ]
  p' (Just Internal{}) = False
  p' Nothing = False
  p' _ = True

type PContext = (Env, Block, Maybe (NS, [PNode], Lambda NodeRef))
evalNodeRef' :: PContext -> NodeRef -> ConstVal
evalNodeRef' (env,block,_) = fromRight' . evalNodeRef env block
evalShape' :: PContext -> [(NodeRef,NodeRef)] -> [(Integer,Integer)]
evalShape' (env,block,_) = evalShape env block

samplePNodes :: PContext -> [(Id, PNode)] -> IO Env
samplePNodes (env,_,_) [] = return env
samplePNodes ctx@(env,block,etc) ((ident,node):rest) = do
    val <- samplePNode ctx node
    let env' = evalBlock block $ Map.insert (LVar ident) val env
    samplePNodes (env', block, etc) rest

samplePNode :: PContext -> PNode -> IO ConstVal
samplePNode ctx d@(Dist f js (SubrangeT t lo hi)) = do
  x <- samplePNode ctx (Dist f js t)
  if any (< lo') (elems' x) || any (hi' <) (elems' x)
    then trace ("rejecting OOB sample "++ show x) $
           samplePNode ctx d
    else return x
  where lo' | (Just r) <- lo = evalNodeRef' ctx r
            | otherwise = negativeInfinity
        hi' | (Just r) <- hi = evalNodeRef' ctx r
            | otherwise = infinity
samplePNode ctx (Dist "bernoulli" [p] _) = fromBool <$> bernoulli p'
  where p' = toDouble $ evalNodeRef' ctx p
samplePNode ctx (Dist "bernoulliLogit" [l] _) = fromBool <$> bernoulli p'
  where l' = toDouble $ evalNodeRef' ctx l
        p' = 1 / (1 + exp (-l'))
samplePNode ctx (Dist "bernoulliLogits" [l] _) = do
  z <- mapM bernoulliLogit l'
  return $ fromList (map fromBool z)
  where l' = map toDouble . toList $ evalNodeRef' ctx l
samplePNode ctx (Dist "pmf" [q] _) = fromInteger <$> pmf q'
  where q' = map toDouble . toList $ evalNodeRef' ctx q
samplePNode ctx (Dist "cauchy" [m,s] _) = fromDouble <$> cauchy m' s'
  where m' = toDouble $ evalNodeRef' ctx m
        s' = toDouble $ evalNodeRef' ctx s
samplePNode ctx (Dist "cauchys" [m,s] (ArrayT _ [_] _)) = fromVector <$> cauchys m' s'
  where m' = toVector $ evalNodeRef' ctx m
        s' = toVector $ evalNodeRef' ctx s
samplePNode ctx (Dist "gamma" [a,b] _) = fromDouble <$> gamma a' b'
  where a' = toDouble $ evalNodeRef' ctx a
        b' = toDouble $ evalNodeRef' ctx b
samplePNode ctx (Dist "inv_gamma" [a,b] _) = fromDouble <$> invGamma a' b'
  where a' = toDouble $ evalNodeRef' ctx a
        b' = toDouble $ evalNodeRef' ctx b
samplePNode ctx (Dist "inv_wishart" [n,w] _) = fromMatrix <$> invWishart n' w'
  where n' = toInteger $ evalNodeRef' ctx n
        w' = toMatrix $ evalNodeRef' ctx w
samplePNode ctx (Dist "geometric" [p] _) = fromInteger <$> geometric 0 p'
  where p' = toDouble $ evalNodeRef' ctx p
samplePNode ctx (Dist "lkj_corr" [v] (ArrayT _ sh _)) = fromMatrix <$> corrLKJ v' (head sh')
  where v' = toDouble $ evalNodeRef' ctx v
        sh' = evalShape' ctx sh
samplePNode ctx (Dist "logistic" [m,s] _) = fromDouble <$> logistic m' s'
  where m' = toDouble $ evalNodeRef' ctx m
        s' = toDouble $ evalNodeRef' ctx s
samplePNode ctx (Dist "neg_binomial" [a,b] _) = fromInteger <$> negBinomial a' b'
  where a' = toDouble $ evalNodeRef' ctx a
        b' = toDouble $ evalNodeRef' ctx b
samplePNode ctx (Dist "normal" [m,s] _) = fromDouble <$> normal m' s'
  where m' = toDouble $ evalNodeRef' ctx m
        s' = toDouble $ evalNodeRef' ctx s
samplePNode ctx (Dist "normals" [m,s] (ArrayT _ [_] _)) = fromVector <$> normals m' s'
  where m' = toVector $ evalNodeRef' ctx m
        s' = toVector $ evalNodeRef' ctx s
samplePNode ctx (Dist "normals" [m,s] (ArrayT _ [_,_] _)) = fromMatrix <$> normals m' s'
  where m' = toMatrix $ evalNodeRef' ctx m
        s' = toMatrix $ evalNodeRef' ctx s
samplePNode ctx (Dist "multi_normal" [m,s] _) = do
  w <- sequence [ normal 0 1 | _ <- [1..n] ]
  let w' = fromList $ map fromDouble w
  return $ m' + chol s' #> w'
  where m' = evalNodeRef' ctx m
        s' = evalNodeRef' ctx s
        n = length (toList m')
samplePNode ctx (Dist "poisson" [a] _) = fromInteger <$> poisson a'
  where a' = toDouble $ evalNodeRef' ctx a
samplePNode ctx (Dist "uniform" [a,b] _) = fromDouble <$> uniform a' b'
  where a' = toDouble $ evalNodeRef' ctx a
        b' = toDouble $ evalNodeRef' ctx b
samplePNode ctx (Dist "uniforms" [a,b] (ArrayT _ sh _)) = do
  z <- zipWithM uniform a' b'
  return $ listArray' (evalShape' ctx sh) (map fromDouble z)
  where a' = map toDouble . elems' $ evalNodeRef' ctx a
        b' = map toDouble . elems' $ evalNodeRef' ctx b
samplePNode ctx (Dist "discreteUniform" [a,b] _) = fromInteger <$> uniform a' b'
  where a' = toInteger $ evalNodeRef' ctx a
        b' = toInteger $ evalNodeRef' ctx b
samplePNode ctx (Dist "wishart" [n,v] _) = fromMatrix <$> wishart n' v'
  where n' = toInteger $ evalNodeRef' ctx n
        v' = toMatrix $ evalNodeRef' ctx v

samplePNode ctx@(_,_,Just (ns,refs,lam)) (Dist "unfold" [seed] _) =
  sampleLambda ctx ns refs lam [seed']
  where seed' = evalNodeRef' ctx seed

samplePNode ctx@(env,block,etc) (Loop shp (Lambda ldag hd) _)
  | isInfinite `any` his = error "cannot sample infinite loop" -- TODO: implement lazily
  | otherwise = listArray' (evalShape' ctx shp) <$> sequence arr
  where his = evalNodeRef' ctx <$> map snd shp
        block' = deriveBlock ldag block
        arr = [ let env' = Map.fromList (inputsL ldag `zip` idx) `Map.union` env
                in samplePNode (env', block', etc) hd
              | idx <- evalRange env block shp ]

samplePNode ctx (HODist "orderedSample" d [n] _) =
  fromList . sort <$> sequence [samplePNode ctx d | _ <- [1..n']]
  where n' = toInteger $ evalNodeRef' ctx n

samplePNode ctx (Switch hd alts ns _) =
  constTuple <$> sampleLambda' ctx ns refs lam ks
  where Tagged c ks = evalNodeRef' ctx hd
        (lam, refs) = alts !! c
        constTuple [x] = x
        constTuple xs = Tagged 0 xs

samplePNode ctx (Chain (lo,hi) refs lam x ns _) =
  chainRange (lo',hi') f x'
  where lo' = integer $ evalNodeRef' ctx lo
        hi' = integer $ evalNodeRef' ctx hi
        x' = evalNodeRef' ctx x
        f i x = sampleLambda ctx ns refs lam [i,x]

samplePNode ctx@(env,block,_) (UnfoldP refs lam seed ns _) =
  sampleLambda ctx' ns refs lam [seed']
  where seed' = evalNodeRef' ctx seed
        ctx' = (env, block, Just (ns,refs,lam))

samplePNode _ d = error $ "samplePNode: unrecognised distribution "++ show d

sampleLambda :: PContext -> NS -> [PNode] -> Lambda NodeRef -> [ConstVal] -> IO ConstVal
sampleLambda ctx ns refs (Lambda dag ret) args =
  head <$> sampleLambda' ctx ns refs (Lambda dag [ret]) args
sampleLambda' :: PContext -> NS -> [PNode] -> Lambda [NodeRef] -> [ConstVal] -> IO [ConstVal]
sampleLambda' (env,block,etc) ns refs (Lambda dag rets) args = do
  let block' = deriveBlock dag block
      idents = [ (Volatile ns (dagLevel dag) i, d) | (i,d) <- zip [0..] refs ]
      env1 = Map.fromList (inputsL dag `zip` args) `Map.union` env
  env2 <- samplePNodes (env1, block', etc) idents
  let p (Just Internal{}) = False
      p Nothing = False
      p _ = True
      env3 = Map.filterWithKey (const . p . getId') env2
  return . fromRight' . sequence $ evalNodeRef env3 block' <$> rets


------------------------------------------------------------------------------
-- DISTRIBUTION CONSTRUCTORS                                                --
------------------------------------------------------------------------------

-- | like 'chain'' but for arbitrary monads
chain :: Monad m => Int -> (b -> m b) -> b -> m b
chain n f = foldr (<=<) return (replicate n f)

-- | infinite loop
loop :: Monad m => a -> (a -> m a) -> m ()
loop s f = do
  s' <- f s
  loop s' f

-- | like 'chainRange'' but for arbitrary monads
chainRange :: (Num i, Monad m) => (Int,Int) -> (i -> x -> m x) -> x -> m x
chainRange (lo,hi) f x0 = snd <$> chain (hi-lo+1) g (integer lo, x0)
  where g (i,x) = do
          y <- f i x
          return (i+1,y)

{-
-- | Metropolis-Hastings inference
mh :: (ExprTuple r, Show r) => P r -> (r -> P r) -> r -> IO r
mh = mhAdjust (const $ LF.logFloat 1)

mhAdjust :: (ExprTuple r, Show r) => (r -> LF.LogFloat) -> P r -> (r -> P r) -> r -> IO r
mhAdjust adjust target proposal x = do
  y <- sampleP (proposal x)
  putStrLn $ "proposing "++ show y
  let f = density target
      q = density' . proposal
      b = (f y * adjust y) / (f x * adjust x)
      c = q x y / q y x
      a = LF.fromLogFloat (b / c) -- (f y * q y x) / (f x * q x y)
  putStrLn $ "acceptance ratio = "++ show b ++" / "++ show c ++" = "++ show a
  accept <- bernoulli $ if a > 1 then 1 else a
  return $ if accept then y else x
-}

-- | like 'rjmc' but assumes the Jacobian factor is equal to one
rjmc1 :: (ExprType t, ExprTuple t, IfB t, BooleanOf t ~ B, Show t)
    => P t -> (t -> P t) -> t -> P t
rjmc1 target proposal x = do
  let a = rjmc1Ratio target proposal
  y <- debug "proposing" <$> proposal x
  accept <- bernoulli (min' 1 (debug "acceptance ratio" $ a x y))
  return $ ifB accept y x

-- | acceptance ratio for 'rjmc1'
rjmc1Ratio :: (ExprTuple t, Show t) => P t -> (t -> P t) -> t -> t -> R
rjmc1Ratio target proposal x y = optimiseE 2 . subst (substEEnv substEnv) $
  exp $ f y - f x + q y' x' - q x' y'
  where f = lpdf target
        q = lpdfAux . proposal
        x' = constSymbolLike "mhx" x
        y' = constSymbolLike "mhy" y
        substEnv = EEnv $ Map.fromList
          [(LVar $ Symbol "mhx" True, erase $ detuple x)
          ,(LVar $ Symbol "mhy" True, erase $ detuple y)
          ]

-- | Reversible Jump Monte Carlo inference
rjmc :: (ExprTuple t, IfB t, BooleanOf t ~ B, Show t) => P t -> (t -> P t) -> t -> P t
rjmc target proposal x = do
  let a = rjmcRatio target proposal
  y <- proposal x
  accept <- bernoulli (min' 1 (a x y))
  return $ ifB accept y x

-- | like 'rjmc' but for ADTs that implement the 'Constructor' typeclass
rjmcC :: (Constructor t, Show t) => P (Expression t) -> (t -> P (Expression t)) -> Expression t -> P (Expression t)
rjmcC target proposal = switchOf $ \x -> do
  y <- fromCaseP proposal x
  accept <- bernoulli . min' 1 . exp $
    f y - f x + optimiseE 2 (rjmcTransRatio' (fromCaseP proposal) x y)
  return $ ifB accept y x
  where f = lpdfAux target -- TODO: jacobian adjustment for transformed dist

rjmcRatio :: (ExprTuple t, Show t) => P t -> (t -> P t) -> t -> t -> R
rjmcRatio target proposal x y = exp $ f y - f x + rjmcTransRatio proposal x y
  where f = lpdfAux target -- TODO: jacobian adjustment for transformed dist

rjmcTransRatio :: (ExprTuple t, Show t) => (t -> P t) -> t -> t -> R
rjmcTransRatio q x y = optimiseE 2 . subst (substEEnv substEnv) $
  rjmcTransRatio' q (constSymbolLike "rjx" x) (constSymbolLike "rjy" y)
  where substEnv = EEnv $ Map.fromList
          [(LVar $ Symbol "rjx" True, erase $ detuple x)
          ,(LVar $ Symbol "rjy" True, erase $ detuple y)
          ]

rjmcTransRatio' :: forall t. (ExprTuple t, Show t) => (t -> P t) -> t -> t -> R
rjmcTransRatio' q x y = lu' - lu + logDet jacobian
  where lu  = q x `lpdfAux` y
        lu' = q y `lpdfAux` x
        getAux ns a b allowInt =
          let (rets, pb) = runProgExprs ns (q a)
              EEnv env = solveTuple (definitions pb) rets b emptyEEnv
              p (LVar i@(Volatile ns' _ _)) _
                | ns' == ns, Just pn' <- pn = allowInt || not (isDiscrete (typePNode pn'))
                where pn = Map.lookup i $ pnodes pb
              p _ _ = False
          in EEnv $ Map.filterWithKey p env
        EEnv u' = getAux "qy" y x False
        uDef = getAux "qx" x y False
        u = do
          let (_,PBlock _ refs _ ns) = runProgExprs "qx" (q x)
          (i,r) <- [0..] `zip` reverse refs
          let t = typePNode r
          Monad.guard $ not (isDiscrete t)
          return $ Var (Volatile ns 0 i) t
        x' = let (rets, pb) = runProgExprs "qx" (q x)
                 tup = toExprTuple $ reDExpr emptyEEnv (definitions pb) <$> rets :: t
             in erase $ detuple tup
        qualify r val = DExpr $ do
          let Just id = getId r
          case lookupEEnv id uDef of
            Just e -> do
              i <- fromDExpr e
              j <- fromDExpr val
              case i of
                Cond cvs _ -> return $ Cond [(c,j) | (c,_) <- cvs] (typeRef j)
                _ -> return j
            Nothing -> trace (show id ++" not in "++ show uDef) $ do
              t <- typeDExpr val
              return $ Unconstrained t
        d_ = derivD emptyEEnv
        d a = d_ a . fst . runExpr . detuple
        top =   d x' x                     : [qualify r $ d_ x' r          | r <- u]
        bot = [(d v x + (d v y <> d x' x)) : [qualify r $ d v y <> d_ x' r | r <- u]
              | v <- Map.elems u']
        substAux = getAux "qx" x y True `unionEEnv` getAux "qy" y x True
        jacobian = Expression . optimiseD 2 . substD (substEEnv substAux) . optimiseD 1 $ blockMatrix (top:bot) :: RMat

-- | sample probabilistic program via Stochaskell's interpreter
runStep :: forall t. (ExprTuple t) => (t -> P t) -> t -> IO t
runStep step = trace (("[runStep]\n"++) . showPBlock stepPB $ show stepRets) $ \m ->
  let env = Map.fromList $ (LVar . Dummy 9 <$> [0..]) `zip` fromRight' (evalTuple emptyEnv m)
  in fromConstVals <$> samplePBlock env stepPB stepRets
  where TypesIs ts = typesOf :: TypesOf t
        dummy :: t
        dummy | [t] <- ts = toExprTuple [DExpr . return $ Var (Dummy 9 0) t]
              | otherwise = entuple . expr . return $
          Data 0 [Var (Dummy 9 i) t | (i,t) <- zip [0..] ts] (TupleT ts)
        (stepRets, stepPB) = runProgExprs "sim" $ step dummy
