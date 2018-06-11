{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables,
             FlexibleContexts, TypeFamilies, MultiParamTypeClasses,
             MonadComprehensions, GeneralizedNewtypeDeriving #-}
module Data.Program where

import Prelude hiding (isInfinite)

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
           | ITDist { dDefs :: DAG
                    , dBase :: [PNode]
                    , dRets :: [NodeRef]
                    , dInvF :: [DExpr]
                    , dInvJ :: [[DExpr]] -- TODO: just the determinant
                    , typePNode :: Type
                    }
           | Switch { sHead :: NodeRef
                    , sAlts :: [(Lambda [NodeRef], [PNode])]
                    , sNS :: NS
                    , typePNode :: Type
                    }
           deriving (Eq)

dependsPNode :: Block -> PNode -> Set Id
dependsPNode block (Dist _ args _) =
  Set.unions $ map (dependsNodeRef block) args
dependsPNode block (Loop sh (Lambda defs dist) _) =
  Set.unions $ map (d . fst) sh ++ map (d . snd) sh ++ [ddeps]
  where d = dependsNodeRef block
        ddeps = Set.filter ((dagLevel defs >) . idLevel) $
          dependsPNode (deriveBlock defs block) dist

instance Show PNode where
  show (Dist d js t) = unwords (d : map show js) ++" :: P "++ show t
  show (Loop sh (Lambda dag hd) t) = "\n"++
    "  [ "++ (drop 4 . indent . indent $ showLet dag hd) ++"\n"++
    "  | "++ intercalate ", " (zipWith g (inputs dag) sh) ++" ] :: "++ show t
    where g i (a,b) = show i ++" <- "++ show a ++"..."++ show b
  show (HODist d j0 js t) = unwords (d : ("("++ show j0 ++")") : map show js) ++" :: P "++ show t
  show (ITDist defs base rets invf invj t) = "ITDist"++
    show (defs, reverse base, rets, reverse invf, invj, t)
  show (Switch e alts ns _) = "switch "++ show e ++" of\n"++ indent cases
    where cases = unlines $ do
            (i, (Lambda dag ret, refs)) <- zip [0..] alts
            let lhs | typeRef e == IntT = show (i+1)
                    | otherwise = "C"++ show i ++" "++ intercalate " " (map show $ inputs dag)
                rhs = indent . showLet' dag . showPNodes ns (dagLevel dag) refs $ show ret
            return $ lhs ++" ->\n"++ rhs

data PBlock = PBlock { definitions :: Block
                     , actions     :: [PNode]
                     , constraints :: Env
                     , namespace   :: String
                     }
            deriving (Eq)
emptyPBlock :: NS -> PBlock
emptyPBlock = PBlock emptyBlock [] emptyEnv

pnodes :: PBlock -> Map Id PNode
pnodes (PBlock _ refs _ ns) = pnodes' ns 0 $ reverse refs
pnodes' :: NS -> Level -> [PNode] -> Map Id PNode
pnodes' ns d = Map.fromList . zip (Volatile ns d <$> [0..])

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

newtype Prog t = Prog { fromProg :: State PBlock t }
  deriving (Functor,Applicative,Monad)
type P t = Prog t
instance (Eq t) => Eq (Prog t) where p == q = runProg "eq" p == runProg "eq" q
runProg :: NS -> Prog a -> (a, PBlock)
runProg ns p = runState (fromProg p) $ emptyPBlock ns

instance (ExprTuple t) => Show (Prog t) where
  show p = showPBlock pb $ show rets
    where (rets, pb) = runProgExprs "show" p

fromProgExprs :: (ExprTuple t) => Prog t -> State PBlock [NodeRef]
fromProgExprs p = do
  es <- fromExprTuple <$> fromProg p
  mapM (liftExprBlock . fromDExpr) es

runProgExprs :: (ExprTuple t) => NS -> Prog t -> ([NodeRef], PBlock)
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

evalProg :: (ExprTuple t) => Env -> Prog t -> Either String t
evalProg env prog = do
  xs <- sequence (evalNodeRef (Map.union given env) block <$> rets)
  return $ fromConstVals xs
  where (rets, PBlock block _ given _) = runProgExprs "eval" prog

caseP :: Int -> DExpr -> [[DExpr] -> P [DExpr]] -> P [DExpr]
caseP n e ps = Prog $ do
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
    return $ do
      block <- get
      let s = do
            r <- fromProg $ p args
            liftExprBlock . sequence $ fromDExpr <$> r
          (ret, PBlock (Block (dag:block')) acts _ _) = runState s $
            PBlock (deriveBlock (DAG d ids Bimap.empty) block) [] emptyEnv ns
      put $ Block block'
      return (Lambda dag ret, reverse acts)
  return $ Switch k cases ns (tupleT . foldr1 (zipWith E.coerce) $ map typeRef . fHead . fst <$> cases)

fromCaseP :: forall c t. (Constructor c, ExprTuple t) => (c -> P t) -> Expr c -> P t
fromCaseP p e = toExprTuple <$> caseP n (erase e)
  [fmap fromExprTuple . p . construct Expr c | c <- cs]
  where Tags cs = tags :: Tags c
        TupleSize n = tupleSize :: TupleSize t

switchOf :: (Constructor c, ExprTuple t) => (Expr c -> P t) -> Expr c -> P t
switchOf f = fromCaseP (f . fromConcrete)

mixture :: forall t. (ExprTuple t) => [(R, P t)] -> P t
mixture qps = do
  k <- categorical qv :: P Z
  toExprTuple <$> caseP n (erase k) (const . fmap fromExprTuple <$> progs)
  where (qs,progs) = unzip qps
        qv = blockVector [cast q | q <- qs] :: RVec
        TupleSize n = tupleSize :: TupleSize t

-- flatten and select with case rather than switch
mixture' :: forall t. (ExprTuple t) => [(R, P t)] -> P t
mixture' qps = do
  k <- categorical qv :: P Z
  rs <- sequence progs
  return . toExprTuple . entupleD n $ caseD (erase k) (const . fromExprTuple <$> rs)
  where (qs,progs) = unzip qps
        qv = blockVector [cast q | q <- qs] :: RVec
        TupleSize n = tupleSize :: TupleSize t


------------------------------------------------------------------------------
-- PRIMITIVE DISTRIBUTIONS                                                  --
------------------------------------------------------------------------------

dist :: State Block PNode -> Prog (Expr t)
dist = dist' . const
dist' :: (NS -> State Block PNode) -> Prog (Expr t)
dist' s = Expr <$> distD s
distD :: (NS -> State Block PNode) -> Prog DExpr
distD s = Prog $ do
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
distDs :: Int -> (NS -> State Block PNode) -> Prog [DExpr]
distDs n s = Prog $ do
  e <- fromProg $ distD s
  return $ if n == 1 then [e] else extractD e 0 <$> [0..(n-1)]

truncated :: (Expr t) -> (Expr t) -> P (Expr t) -> P (Expr t)
truncated a b p = Prog $ do
  i <- liftExprBlock $ fromExpr a
  j <- liftExprBlock $ fromExpr b
  x <- fromProg p
  (Var name t) <- liftExprBlock $ fromExpr x
  PBlock block (d:rhs) given ns <- get
  when (name /= Volatile ns (dagLevel $ topDAG block) (length rhs)) $
    error "truncated: program does not appear to be primitive"
  let g k | (Const c _) <- k, isInfinite c = Nothing
          | otherwise = Just k
      t' = SubrangeT t (g i) (g j)
      d' = d { typePNode = t' }
  put $ PBlock block (d':rhs) given ns
  return (expr $ return (Var name t'))

transform :: (ExprTuple t) => Prog t -> Prog t
transform prog = Prog $ do
  PBlock block acts given ns <- get
  assert (given == emptyEnv) $ return ()
  let d = nextLevel block
      dBlock = deriveBlock (DAG d [] Bimap.empty) block
      (rets, PBlock dBlock'@(Block (dag:block')) acts' _ _) =
        runState (fromProgExprs prog) $ PBlock dBlock [] emptyEnv ns
      ids = Dummy (d-1) <$> [0..(length rets - 1)]
      zs = zipWith Var ids $ map typeRef rets
      eenv = solveTupleD dBlock' rets (DExpr . return <$> zs) emptyEEnv
      invfs = [fromMaybe (error "not invertible") $ Map.lookup (LVar x) eenv
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

instance Distribution Bernoulli R Prog B where
    sample (Bernoulli p) = dist $ do
        i <- fromExpr p
        return $ Dist "bernoulli" [i] boolT
    sample (BernoulliLogit l) = dist $ do
        i <- fromExpr l
        return $ Dist "bernoulliLogit" [i] boolT

instance Distribution Bernoullis RVec Prog BVec where
    sample (Bernoullis p) = dist $ do
        i <- fromExpr p
        let (ArrayT _ [n] _) = typeRef i
        return $ Dist "bernoullis" [i] (ArrayT Nothing [n] boolT)
    sample (BernoulliLogits l) = dist $ do
        i <- fromExpr l
        let (ArrayT _ [n] _) = typeRef i
        return $ Dist "bernoulliLogits" [i] (ArrayT Nothing [n] boolT)

instance Distribution Beta (R,R) Prog R where
    sample (Beta (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "beta" [i,j] RealT

instance Distribution Categorical RVec Prog Z where
    sample (Categorical q) = dist $ do
        i <- fromExpr q
        return $ Dist "categorical" [i] IntT

instance Distribution Cauchy (R,R) Prog R where
    sample (Cauchy (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "cauchy" [i,j] RealT

instance Distribution Cauchys (RVec,RVec) Prog RVec where
    sample (Cauchys (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "cauchys" [i,j] (typeRef i)

instance Distribution Gamma (R,R) Prog R where
    sample (Gamma (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "gamma" [i,j] RealT

instance Distribution InvGamma (R,R) Prog R where
    sample (InvGamma (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "inv_gamma" [i,j] RealT

instance Distribution Geometric R Prog Z where
    sample (Geometric p) = dist $ do
        i <- fromExpr p
        return $ Dist "geometric" [i] IntT

instance Distribution LKJ (R, Interval Z) Prog RMat where
    sample (LKJ (v,(a,b))) = dist $ do
        i <- fromExpr v
        l <- fromExpr a
        h <- fromExpr b
        return $ Dist "lkj_corr" [i] (ArrayT (Just "corr_matrix") [(l,h),(l,h)] RealT)

instance Distribution NegBinomial (R,R) Prog Z where
    sample (NegBinomial (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "neg_binomial" [i,j] IntT

instance Distribution Normal (R,R) Prog R where
    sample (Normal (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normal" [i,j] RealT

instance Distribution Normals (RVec,RVec) Prog RVec where
    sample (Normals (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normals" [i,j] (typeRef i)

instance Distribution Normals (RMat,RMat) Prog RMat where
    sample (Normals (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normals" [i,j] (typeRef i)

instance Distribution Normal (RVec,RMat) Prog RVec where
    sample (Normal (m,s)) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "multi_normal" [i,j] (typeRef i)

instance (ScalarType t) => Distribution OrderedSample (Z, Prog (Expr t)) Prog (Expr [t]) where
    sample (OrderedSample (n,prog)) = Prog $ do
        i <- liftExprBlock $ fromExpr n
        PBlock block rhs given ns <- get
        let (_, PBlock block' [act] _ _) =
              runState (head <$> fromProgExprs prog) $ PBlock block [] emptyEnv ns
            d = HODist "orderedSample" act [i] (ArrayT Nothing [(Const 1 IntT,i)] (typePNode act))
        put $ PBlock block' (d:rhs) given ns
        let depth = dagLevel $ topDAG block
            k = length rhs
            name = Volatile ns depth k
            t = typePNode d
            v = Var name t
        _ <- liftExprBlock . simplify $ Apply "getExternal" [v] t
        return (expr $ return v)

instance Distribution PMF RVec Prog Z where
    sample (PMF probs) = dist $ do
        l <- fromExpr probs
        return $ Dist "pmf" [l] IntT

instance Distribution Poisson R Prog Z where
    sample (Poisson a) = dist $ do
        i <- fromExpr a
        return $ Dist "poisson" [i] IntT

instance Distribution PoissonProcess (R, R -> R, R) Prog RVec where
    sample (PoissonProcess (t, rate, mean)) = dist $ do
        i <- fromExpr t
        d <- getNextLevel
        lam <- runLambda [(Dummy d 1, RealT)] . fromExpr . rate . expr . return $
          Var (Dummy d 1) RealT
        j <- simplify $ Function lam RealT
        k <- fromExpr mean
        return . Dist "poissonProcess" [i,j,k] $
          ArrayT (Just "vector") [(Const 1 IntT, Unconstrained IntT)] RealT

instance Distribution Uniform (R,R) Prog R where
    sample (Uniform (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniform" [i,j] RealT

instance Distribution Uniforms (RVec,RVec) Prog RVec where
    sample (Uniforms (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniforms" [i,j] (typeRef i)

instance Distribution Uniforms (RMat,RMat) Prog RMat where
    sample (Uniforms (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "uniforms" [i,j] (typeRef i)

instance Distribution Uniform (Z,Z) Prog Z where
    sample (Uniform (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "discreteUniform" [i,j] IntT

instance Distribution Wishart (R,RMat) Prog RMat where
    sample (Wishart (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        return $ Dist "wishart" [i,j] (typeRef j)

instance Distribution InvWishart (R,RMat) Prog RMat where
    sample (InvWishart (a,b)) = dist $ do
        i <- fromExpr a
        j <- fromExpr b
        let (ArrayT _ sh t) = typeRef j
        return $ Dist "inv_wishart" [i,j] (ArrayT (Just "cov_matrix") sh t)

normalChol :: Z -> RVec -> RMat -> P RVec
normalChol n mu cov = do
  --w <- joint vector [ normal 0 1 | _ <- 1...n ]
  w <- normals (vector [ 0 | _ <- 1...n ])
               (vector [ 1 | _ <- 1...n ])
  return (mu + chol cov #> w)

normalsChol :: Z -> Z -> RVec -> RMat -> P RMat
normalsChol n k mu cov = do
  --w <- joint vector [ normal 0 1 | i <- 1...n, j <- 1...k ]
  w <- normals (matrix [ 0 | i <- 1...n, j <- 1...k ])
               (matrix [ 1 | i <- 1...n, j <- 1...k ])
  return $ asRow mu + (w <> tr' (chol cov))

normalCond :: Z -> (Expr t -> Expr t -> R) -> Expr [t] -> RVec -> Expr t -> P R
normalCond n cov s y x = normal m (sqrt v)
  where c = matrix [ cov (s!i) (s!j) | i <- 1...n, j <- 1...n ] :: RMat
        k = vector [ cov (s!i) x     | i <- 1...n ] :: RVec
        m = y <.> (inv c #> k)
        v = cov x x - k <.> (inv c #> k)


------------------------------------------------------------------------------
-- LOOPS                                                                    --
------------------------------------------------------------------------------

instance forall r f. ScalarType r =>
         Joint Prog Z (Expr r) (Expr f) where
  joint _ ar = Prog $ do
    sh <- liftExprBlock . sequence . flip map (shape ar) $ \(a,b) -> do
      i <- fromExpr a
      j <- fromExpr b
      return (i,j)
    PBlock block dists given ns <- get
    let d = nextLevel block
        ids = [(Dummy d i, IntT) | i <- [1..length sh]]
        p = ar ! [expr . return $ Var i t | (i,t) <- ids]
        (ret, PBlock (Block (dag:block')) [act] _ _) =
          runState (head <$> fromProgExprs p) $
            PBlock (deriveBlock (DAG d ids Bimap.empty) block) [] emptyEnv ns
        TypeIs t = typeOf :: TypeOf r -- TODO: incorrect type for transformed case
        loopType = ArrayT Nothing sh t
        loop = Loop sh (Lambda dag act) loopType
    put $ PBlock (Block block') (loop:dists) given ns
    let name = Volatile ns (d-1) (length dists)
        v = Var name loopType
    _ <- liftExprBlock . simplify $ Apply "getExternal" [v] loopType
    return $ case ret of
      Var (Volatile ns depth 0) _ | depth == d ->
        expr $ return v :: Expr f
      Index vec [Var (Volatile ns depth 0) _] | depth == d ->
        expr . floatNode $ Array sh (Lambda dag (Index vec [ref])) loopType
          where ref = Index v (reverse [Var i t | (i,t) <- ids])
      _ -> error $ "non-trivial transform in joint: "++ show ret


------------------------------------------------------------------------------
-- CONDITIONING                                                             --
------------------------------------------------------------------------------

type instance ConditionOf (Prog ()) = Expr Bool
instance MonadGuard Prog where
    guard cond = Prog $ do -- TODO: weaker assumptions
        (Var (Internal 0 i) _) <- liftExprBlock (fromExpr cond)
        PBlock block dists given ns <- get
        let dag = topDAG block
        assert (i == length (nodes dag) - 1) $ return ()
        let (Just (Apply "==" [Var j t, Const a _] _)) =
              lookup i $ nodes dag
            dag' = dag { bimap = Bimap.deleteR i (bimap dag) }
        put $ PBlock (deriveBlock dag' block) dists (Map.insert (LVar j) a given) ns

dirac :: (Expr t) -> Prog (Expr t)
dirac c = do
  x <- dist $ do
    i <- fromExpr c
    return $ Dist "dirac" [i] (typeRef i)
  guard $ x ==* c
  return x


------------------------------------------------------------------------------
-- PROBABILITY DENSITIES                                                    --
------------------------------------------------------------------------------

pdf :: (ExprTuple t) => Prog t -> t -> R
pdf prog vals = subst (substEEnv env') $ pdfPBlock False env pb
  where (rets, pb) = runProgExprs "pdf" prog
        env = solveTuple (definitions pb) rets vals emptyEEnv
        env' = Map.filterWithKey p env
        p (LVar (Volatile "pdf" _ _)) _ = True
        p _ _ = False

pdfC :: (Constructor t) => Prog (Expr t) -> Expr t -> R
pdfC = caseOf . pdf

lpdf :: (ExprTuple t, Show t) => Prog t -> t -> R
lpdf prog vals = subst (substEEnv env') $ pdfPBlock True env pb
  where (rets, pb) = runProgExprs "lpdf" prog
        env = solveTuple (definitions pb) rets vals emptyEEnv
        env' = Map.filterWithKey p env
        p (LVar (Volatile "lpdf" _ _)) _ = True
        p _ _ = False

lpdfC :: (Constructor t, Show t) => Prog (Expr t) -> Expr t -> R
lpdfC = caseOf . lpdf

pdfPBlock :: Bool -> EEnv -> PBlock -> R
pdfPBlock lg env pb@(PBlock block refs _ ns) = pdfPNodes (pnodes pb) ns lg env block refs

pdfPNodes :: Map Id PNode -> NS -> Bool -> EEnv -> Block -> [PNode] -> R
pdfPNodes r ns lg env block refs = (if lg then sum else product) $ do
    (i,d) <- zip [0..] $ reverse refs
    let ident = Volatile ns (dagLevel $ topDAG block) i
    return $ case Map.lookup (LVar ident) env of
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
pdfPNode r lg env block (Loop _ (Lambda ldag body) _) a
  | (Unconstrained _,_) <- runDExpr a = if lg then 0 else 1
  | (PartiallyConstrained{},_) <- runDExpr a = error "TODO: partially constrained"
  | lg        = E.foldl g 0 (Expr a)
  | otherwise = E.foldl f 1 (Expr a)
  where block' = deriveBlock ldag block
        f p x = p * pdfPNode r lg env block' body (erase x) -- TODO only works for iid
        g l x = l + pdfPNode r lg env block' body (erase x)
pdfPNode r lg env block (HODist "orderedSample" d [n] _) x = expr $ do
  i <- fromDExpr x
  case i of
    Unconstrained _ -> fromExpr $ if lg then 0 else 1 :: R
    PartiallyConstrained [(lo,hi)] [(id,t)] [([k],v)] _ -> fromExpr $
      pdfOrderStats r lg env block d n (lo,hi) (id,t) (k,v)
    _ -> error $ "pdfPNode orderedSample "++ show x
pdfPNode _ _ _ _ node x = error $ "pdfPNode "++ show node ++" "++ show x

pdfOrderStats :: Map Id PNode -> Bool -> EEnv -> Block -> PNode -> NodeRef
              -> Interval DExpr -> (Id,Type) -> (DExpr,DExpr) -> R
pdfOrderStats r lg env block d n (lo,hi) (dummy,dummyT) (k,v) =
  ifB (Expr lo >* Expr hi) (if lg then 0 else 1) $ if lg
  then    logFactorial' n'  + intervalL +     sum' intervals + intervalR +     sum' points
  else cast (factorial' n') * intervalL * product' intervals * intervalR * product' points
  where fcdf = cdfPNode env block d
        n' = reExpr env block n :: Z
        kv' i = let envi = Map.singleton (LVar dummy) i
                in (Expr $ substD envi k, substD envi v) :: (Z,DExpr)
        intervalL = let (i,x) = kv' lo in if lg
          then log (fcdf x) *  cast (i-1) -    logFactorial' (i-1)
          else     (fcdf x) ** cast (i-1) / cast (factorial' (i-1))
        interval (i,x) (j,y) = if lg
          then log (fcdf y - fcdf x) *  cast (j-i-1) -    logFactorial' (j-i-1)
          else     (fcdf y - fcdf x) ** cast (j-i-1) / cast (factorial' (j-i-1))
        intervals = vector [ interval (kv' $ erase i) (kv' . erase $ i+1)
                           | i::Z <- (Expr lo)...(Expr $ hi-1) ] :: RVec
        intervalR = let (j,y) = kv' hi in if lg
          then log (1 - fcdf y) *  cast (n'-j) -    logFactorial' (n'-j)
          else     (1 - fcdf y) ** cast (n'-j) / cast (factorial' (n'-j))
        points = vector [ let (_,x) = (kv' $ erase i) in pdfPNode r lg env block d x
                        | i::Z <- (Expr lo)...(Expr hi) ] :: RVec
        sum' = E.foldl (+) 0
        product' = E.foldl (*) 1

cdfPNode :: EEnv -> Block -> PNode -> DExpr -> R
cdfPNode env block (Dist f args _) x = expr $ do
  i <- fromDExpr x
  js <- sequence $ extractNodeRef simplifyNodeRef simplify env block <$> args
  simplify $ Apply (f ++"_cdf") (i:js) RealT

density :: (ExprTuple t) => Prog t -> t -> LF.LogFloat
density prog vals = densityPBlock env' pb / adjust
  where (rets, pb@(PBlock block acts _ ns)) = runProgExprs "density" prog
        env = unifyTuple block rets vals emptyEnv
        env' = evalBlock block env
        jacobian = [ [ diffNodeRef env' block r (Volatile ns 0 i) (typePNode d)
                     | (i,d) <- zip [0..] (reverse acts), typePNode d /= IntT ]
                   | r <- rets, typeRef r /= IntT ]
        isLowerTri = and [ isZeros `all` drop i row | (i,row) <- zip [1..] jacobian ]
        diagonal = [ row !! i | (i,row) <- zip [0..] jacobian ]
        ldet = LF.logToLogFloat . real . logDet :: ConstVal -> LF.LogFloat
        adjust | isLowerTri = product (map ldet diagonal)
               | otherwise = error "jacobian is not block triangular"

density' :: (ExprTuple t) => Prog t -> t -> LF.LogFloat
density' prog vals = densityPBlock env' pb
  where (rets, pb) = runProgExprs "density'" prog
        env = unifyTuple (definitions pb) rets vals emptyEnv
        env' = evalBlock (definitions pb) env

densityPBlock :: Env -> PBlock -> LF.LogFloat
densityPBlock env (PBlock block refs _ ns) = product $ do
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
        l' = toDouble . fromRight' $ evalNodeRef env block l
        p = 1 / (1 + exp (-l'))
densityPNode env block (Dist "categorical" [q] _) x = LF.logFloat p
  where q' = map toDouble . toList . fromRight' $ evalNodeRef env block q
        p = fromMaybe 0 . lookup x $ zip [1..] q'
densityPNode env block (Dist "gamma" [a,b] _) x
    | x' >= 0 = LF.logToLogFloat $ lpdfGamma x' a' b'
    | otherwise = LF.logFloat 0
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
        x' = toDouble x
densityPNode env block (Dist "inv_gamma" [a,b] _) x
    | x' >= 0 = LF.logToLogFloat l
    | otherwise = LF.logFloat 0
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
        x' = toDouble x
        l = a' * log b' - (a' + 1) * log x' - b' / x' - logGamma a'
densityPNode env block (Dist "geometric" [t] _) x = p * q^k
  where t' = toDouble . fromRight' $ evalNodeRef env block t
        p = LF.logFloat t'
        q = LF.logFloat (1 - t')
        k = toInteger x
densityPNode env block (Dist "normal" [m,s] _) x =
    LF.logToLogFloat $ logPdf (Rand.Normal m' s') (toDouble x)
  where m' = toDouble . fromRight' $ evalNodeRef env block m
        s' = toDouble . fromRight' $ evalNodeRef env block s
densityPNode env block (Dist "multi_normal" [m,s] _) x =
    LF.logToLogFloat $ -0.5 * (real $ (x' <.> (s' <\> x')) + logDet s' + n * log (2*pi))
  where m' = fromRight' $ evalNodeRef env block m
        s' = fromRight' $ evalNodeRef env block s
        n = integer $ length (toList m')
        x' = x - m'
densityPNode env block (Dist "poisson" [l] _) x =
    LF.logToLogFloat $ lpdfPoisson k l'
  where l' = toDouble . fromRight' $ evalNodeRef env block l
        k = toInteger x
densityPNode env block (Dist "uniform" [a,b] _) x =
    LF.logFloat $ if a' <= x' && x' <= b' then 1/(b' - a') else 0
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
        x' = toDouble x
densityPNode env block (Dist "discreteUniform" [a,b] _) x =
    LF.logFloat $ if a' <= x' && x' <= b' then 1/(fromInteger $ b' - a' + 1) else 0
  where a' = toInteger . fromRight' $ evalNodeRef env block a
        b' = toInteger . fromRight' $ evalNodeRef env block b
        x' = toInteger x
densityPNode _ _ (Dist d _ _) _ = error $ "unrecognised density "++ d

densityPNode env block (Loop shp (Lambda ldag body) _) a = product
    [ let env' = Map.fromList (inputsL ldag `zip` i) `Map.union` env
      in densityPNode env' block' body (fromRational x)
    | (i,x) <- evalRange env block shp `zip` entries a ]
  where block' = deriveBlock ldag block

densityPNode env block (HODist "orderedSample" d [n] _) a = lfact n' * product
    [ densityPNode env block d (fromRational x) | x <- entries a ]
  where n' = toInteger . fromRight' $ evalNodeRef env block n


------------------------------------------------------------------------------
-- SAMPLING                                                                 --
------------------------------------------------------------------------------

simulate :: (ExprTuple t) => Prog t -> IO t
simulate = sampleP

sampleP :: (ExprTuple t) => Prog t -> IO t
sampleP = sampleP' emptyEnv
sampleP' :: (ExprTuple t) => Env -> Prog t -> IO t
sampleP' env p = fromConstVals <$> samplePBlock env pb rets
  where (rets, pb) = runProgExprs "sim" p

samplePBlock :: Env -> PBlock -> [NodeRef] -> IO [ConstVal]
samplePBlock env (PBlock block refs _ ns) rets = do
    env' <- samplePNodes env block idents
    let env'' = Map.filterWithKey (const . p' . getId') env'
    return $ map (fromRight' . evalNodeRef env'' block) rets
  where idents = [ (Volatile ns (dagLevel $ topDAG block) i, d)
                 | (i,d) <- zip [0..] $ reverse refs ]
        p' (Just Internal{}) = False
        p' Nothing = False
        p' _ = True

samplePNodes :: Env -> Block -> [(Id, PNode)] -> IO Env
samplePNodes env _ [] = return env
samplePNodes env block ((ident,node):rest) = do
    val <- samplePNode env block node
    let env' = evalBlock block $ Map.insert (LVar ident) val env
    samplePNodes env' block rest

samplePNode :: Env -> Block -> PNode -> IO ConstVal
samplePNode env block d@(Dist f js (SubrangeT t lo hi)) = do
  x <- samplePNode env block (Dist f js t)
  if flip any (elems' x) (< lo') || any (hi' <) (elems' x)
    then trace ("rejecting OOB sample "++ show x) $
           samplePNode env block d
    else return x
  where lo' | (Just r) <- lo = fromRight' $ evalNodeRef env block r
            | otherwise = negativeInfinity
        hi' | (Just r) <- hi = fromRight' $ evalNodeRef env block r
            | otherwise = infinity
samplePNode env block (Dist "bernoulli" [p] _) = fromBool <$> bernoulli p'
  where p' = toDouble . fromRight' $ evalNodeRef env block p
samplePNode env block (Dist "bernoulliLogit" [l] _) = fromBool <$> bernoulli p'
  where l' = toDouble . fromRight' $ evalNodeRef env block l
        p' = 1 / (1 + exp (-l'))
samplePNode env block (Dist "bernoulliLogits" [l] _) = do
  z <- sequence $ map bernoulliLogit l'
  return $ fromList (map fromBool z)
  where l' = map toDouble . toList . fromRight' $ evalNodeRef env block l
samplePNode env block (Dist "categorical" [q] _) = fromInteger <$> categorical q'
  where q' = map toDouble . toList . fromRight' $ evalNodeRef env block q
samplePNode env block (Dist "cauchy" [m,s] _) = fromDouble <$> cauchy m' s'
  where m' = toDouble . fromRight' $ evalNodeRef env block m
        s' = toDouble . fromRight' $ evalNodeRef env block s
samplePNode env block (Dist "cauchys" [m,s] (ArrayT _ [_] _)) = fromVector <$> cauchys m' s'
  where m' = toVector . fromRight' $ evalNodeRef env block m
        s' = toVector . fromRight' $ evalNodeRef env block s
samplePNode env block (Dist "gamma" [a,b] _) = fromDouble <$> gamma a' b'
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "inv_gamma" [a,b] _) = fromDouble <$> invGamma a' b'
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "inv_wishart" [n,w] _) = fromMatrix <$> invWishart n' w'
  where n' = toInteger . fromRight' $ evalNodeRef env block n
        w' = toMatrix . fromRight' $ evalNodeRef env block w
samplePNode env block (Dist "geometric" [p] _) = fromInteger <$> geometric 0 p'
  where p' = toDouble . fromRight' $ evalNodeRef env block p
samplePNode env block (Dist "lkj_corr" [v] (ArrayT _ sh _)) = fromMatrix <$> corrLKJ v' (head sh')
  where v' = toDouble . fromRight' $ evalNodeRef env block v
        sh' = evalShape env block sh
samplePNode env block (Dist "neg_binomial" [a,b] _) = fromInteger <$> negBinomial a' b'
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "normal" [m,s] _) = fromDouble <$> normal m' s'
  where m' = toDouble . fromRight' $ evalNodeRef env block m
        s' = toDouble . fromRight' $ evalNodeRef env block s
samplePNode env block (Dist "normals" [m,s] (ArrayT _ [_] _)) = fromVector <$> normals m' s'
  where m' = toVector . fromRight' $ evalNodeRef env block m
        s' = toVector . fromRight' $ evalNodeRef env block s
samplePNode env block (Dist "normals" [m,s] (ArrayT _ [_,_] _)) = fromMatrix <$> normals m' s'
  where m' = toMatrix . fromRight' $ evalNodeRef env block m
        s' = toMatrix . fromRight' $ evalNodeRef env block s
samplePNode env block (Dist "multi_normal" [m,s] _) = do
  w <- sequence [ normal 0 1 | _ <- [1..n] ]
  let w' = fromList $ map fromDouble w
  return $ m' + chol s' #> w'
  where m' = fromRight' $ evalNodeRef env block m
        s' = fromRight' $ evalNodeRef env block s
        n = length (toList m')
samplePNode env block (Dist "poisson" [a] _) = fromInteger <$> poisson a'
  where a' = toDouble . fromRight' $ evalNodeRef env block a
samplePNode env block (Dist "uniform" [a,b] _) = fromDouble <$> uniform a' b'
  where a' = toDouble . fromRight' $ evalNodeRef env block a
        b' = toDouble . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "uniforms" [a,b] (ArrayT _ sh _)) = do
  z <- sequence $ zipWith uniform a' b'
  return $ listArray' (evalShape env block sh) (map fromDouble z)
  where a' = map toDouble . elems' . fromRight' $ evalNodeRef env block a
        b' = map toDouble . elems' . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "discreteUniform" [a,b] _) = fromInteger <$> uniform a' b'
  where a' = toInteger . fromRight' $ evalNodeRef env block a
        b' = toInteger . fromRight' $ evalNodeRef env block b
samplePNode env block (Dist "wishart" [n,v] _) = fromMatrix <$> wishart n' v'
  where n' = toInteger . fromRight' $ evalNodeRef env block n
        v' = toMatrix . fromRight' $ evalNodeRef env block v

samplePNode env block (Loop shp (Lambda ldag hd) _) =
  listArray' (evalShape env block shp) <$> sequence arr
  where block' = deriveBlock ldag block
        arr = [ let env' = Map.fromList (inputsL ldag `zip` idx) `Map.union` env
                in samplePNode env' block' hd
              | idx <- evalRange env block shp ]

samplePNode env block (HODist "orderedSample" d [n] _) =
  (fromList . sort) <$> sequence [samplePNode env block d | _ <- [1..n']]
  where n' = toInteger . fromRight' $ evalNodeRef env block n

samplePNode env block (Switch hd alts ns _) = do
  let env' = Map.fromList (inputsL ldag `zip` ks) `Map.union` env
  env'' <- samplePNodes env' block' idents
  let env''' = Map.filterWithKey (const . p . getId') env''
  return . constTuple . fromRight' . sequence $ evalNodeRef env''' block' <$> lhd
  where Tagged c ks = fromRight' $ evalNodeRef env block hd
        (Lambda ldag lhd, ds) = alts !! c
        block' = deriveBlock ldag block
        idents = [ (Volatile ns (dagLevel ldag) i, d) | (i,d) <- zip [0..] ds ]
        constTuple [x] = x
        constTuple xs = Tagged 0 xs
        p (Just Internal{}) = False
        p Nothing = False
        p _ = True

samplePNode _ _ d = error $ "samplePNode: unrecognised distribution "++ show d


------------------------------------------------------------------------------
-- DISTRIBUTION CONSTRUCTORS                                                --
------------------------------------------------------------------------------

-- from hsc3
chain :: Monad m => Int -> (b -> m b) -> b -> m b
chain n f = foldr (<=<) return (replicate n f)
loop :: Monad m => a -> (a -> m a) -> m ()
loop s f = do
  s' <- f s
  loop s' f

chainRange :: (Num i, Monad m) => (Int,Int) -> (i -> x -> m x) -> x -> m x
chainRange (lo,hi) f x0 = snd <$> chain (hi-lo+1) g (integer lo, x0)
  where g (i,x) = do
          y <- f i x
          return (i+1,y)

-- Metropolis-Hastings
mh :: (ExprTuple r, Show r) => Prog r -> (r -> Prog r) -> r -> IO r
mh = mhAdjust (const $ LF.logFloat 1)

mhAdjust :: (ExprTuple r, Show r) => (r -> LF.LogFloat) -> Prog r -> (r -> Prog r) -> r -> IO r
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

rjmc :: (ExprTuple t, IfB t, BooleanOf t ~ B, Show t) => P t -> (t -> P t) -> t -> P t
rjmc target proposal x = do
  y <- proposal x
  let f = lpdf target -- TODO: jacobian adjustment for transformed dist
      a = exp $ f y - f x + rjmcTransRatio proposal x y
  accept <- bernoulli $ min' 1 a
  return $ ifB accept y x

rjmcC :: (Constructor t, Show t) => P (Expr t) -> (t -> P (Expr t)) -> Expr t -> P (Expr t)
rjmcC p = switchOf . rjmc p . fromCaseP

rjmcTransRatio :: forall t. (ExprTuple t, Show t) => (t -> P t) -> t -> t -> R
rjmcTransRatio q x y = subst (substEEnv substEnv) $ lu' - lu + logDet jacobian
  where lu  = q x_ `lpdf` y_
        lu' = q y_ `lpdf` x_
        getAux ns a b allowInt =
          let (rets, pb) = runProgExprs ns (q a)
              env = solveTuple (definitions pb) rets b emptyEEnv
              p (LVar i@(Volatile ns' _ _)) _
                | ns' == ns, Just pn' <- pn = allowInt || typePNode pn' /= IntT
                where pn = Map.lookup i $ pnodes pb
              p _ _ = False
          in Map.filterWithKey p env
        u' = getAux "qy" y_ x_ False
        uDef = getAux "qx" x_ y_ False
        u = do
          let (_,PBlock _ refs _ ns) = runProgExprs "qx" (q x_)
          (i,r) <- [0..] `zip` reverse refs
          let t = typePNode r
          Monad.guard $ t /= IntT
          return $ Var (Volatile ns 0 i) t
        x_::t = entuple . expr $ do
          t <- extractType emptyEEnv block (typeRef ret)
          return $ Var (Symbol "rjx") t
          where (ret, block) = runExpr (detuple x)
        y_::t = entuple . expr $ do
          t <- extractType emptyEEnv block (typeRef ret)
          return $ Var (Symbol "rjy") t
          where (ret, block) = runExpr (detuple y)
        x' = let (rets, pb) = runProgExprs "qx" (q x_)
                 tup = toExprTuple $ reDExpr emptyEEnv (definitions pb) <$> rets :: t
             in erase $ detuple tup
        qualify r val = DExpr $ do
          let Just id = getId r
          case Map.lookup (LVar id) uDef of
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
        top =   d x' x_                       : [qualify r $ d_ x' r           | r <- u]
        bot = [(d v x_ + (d v y_ <> d x' x_)) : [qualify r $ d v y_ <> d_ x' r | r <- u]
              | v <- Map.elems u']
        substAux = getAux "qx" x_ y_ True `Map.union` getAux "qy" y_ x_ True
        jacobian = Expr . optimiseD 2 . substD (substEEnv substAux) . optimiseD 1 $ blockMatrix (top:bot) :: RMat
        substEnv = Map.fromList
          [(LVar $ Symbol "rjx", erase $ detuple x)
          ,(LVar $ Symbol "rjy", erase $ detuple y)
          ]
