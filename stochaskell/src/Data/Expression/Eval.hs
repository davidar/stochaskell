{-# LANGUAGE FlexibleInstances, MonadComprehensions, MultiWayIf, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>),isInfinite)

import Control.Exception
import Control.Monad
import Data.Array.IArray (listArray)
import Data.Array.Abstract
import Data.Boolean
import Data.Either
import Data.Expression hiding (const)
import Data.Expression.Case
import Data.Expression.Const hiding (isScalar)
import Data.Expression.Extract
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Ix
import Data.List
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Number.Transfinite (isInfinite)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Exts
import GHC.Generics hiding (Constructor,R,P)
import Util

type Env = Map LVal ConstVal
emptyEnv :: Env
emptyEnv = Map.empty

-- number of elements in a value of the given type
numelType :: Env -> Block -> Type -> Integer
numelType _ _ IntT = 1
numelType _ _ RealT = 1
numelType env block (SubrangeT t _ _) = numelType env block t
numelType env block (ArrayT _ sh _) = product $ map f sh'
  where sh' = evalShape env block sh
        f (lo,hi) = hi - lo + 1

-- TODO: mark individual symbols as evaluable, even when not set in env
evaluable :: EEnv -> Block -> NodeRef -> Bool
evaluable (EEnv env) block ref =
  deps `Set.isSubsetOf` (Set.fromList . mapMaybe getId' . Set.toList $ Map.keysSet env)
  where deps = Set.filter p $ dependsNodeRef block ref
        p Dummy{}    = True
        p Volatile{} = True
        p Internal{} = False
        p (Symbol _ known) = not known

-- | either an evaluated constant or error string
type Eval = Either String ConstVal
type Evals = Either String [ConstVal]

evalContext :: String -> Either String a -> Either String a
evalContext s (Left e) = Left $ s ++":\n"++ indent e
evalContext _ r = r

eval :: Env -> Expression t -> Eval
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

-- | evaluate Stochaskell expression to constant value
eval_ :: Expression t -> Eval
eval_ = eval emptyEnv

-- | evaluate Stochaskell expression to native value
eval' :: (ExprType t) => Expression t -> Either String t
eval' = fmap toConcrete . eval_

evalD :: Env -> DExpr -> Eval
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalD_ :: DExpr -> Eval
evalD_ = evalD emptyEnv

evalTuple :: (ExprTuple t) => Env -> t -> Evals
evalTuple env = mapM (evalD env) . fromExprTuple

evalEEnv_ :: EEnv -> Env
evalEEnv_ (EEnv env) = Map.fromList $ do
  (k,v) <- Map.toList env
  case evalD_ v of
    Left e -> trace ("evalEEnv_: "++ e) mzero
    Right x -> return (k,x)

evalNodeRefs :: Env -> Block -> [NodeRef] -> Evals
evalNodeRefs env block = mapM (evalNodeRef env block)

evalNodeRef :: Env -> Block -> NodeRef -> Eval
evalNodeRef env _ (Var ident _) | isJust val = Right $ fromJust val
  where val = Map.lookup (LVar ident) env
evalNodeRef env block (Var i@Internal{} _) =
  evalNode env block $ lookupBlock i block
evalNodeRef _ _ r@(Var ident _) = Left $ "unable to eval "++ show r
evalNodeRef _ _ (Const c _) = Right c
evalNodeRef env block r@(Index (PartiallyConstrained [(lo,hi)] [(id,_)] [([k],v)] _) [j]) =
  evalContext (show r) $ do
    let env_ = Map.filterWithKey (const . maybe True (not . isInternal) . getId') env
    lo' <- integer <$> evalD env_ lo
    hi' <- integer <$> evalD env_ hi
    j' <- integer <$> evalNodeRef env block j
    let env' i = Map.insert (LVar id) (fromInteger i) env_
    case lookup (Right j') [(evalD (env' i) k, i) | i <- range (lo',hi')] of
      Just i -> evalD (env' i) v
      Nothing -> Left "insufficient partial constraints"
evalNodeRef env block r@(Index arr idx) = do
  a <- evalNodeRef env block arr
  js <- evalNodeRefs env block $ reverse idx
  let (lo,hi) = bounds a
      js' = toInteger <$> js
      n = length js'
      bounds' = (take n lo, take n hi)
  if bounds' `inRange` js' then return (a!js') else
    Left $ show js' ++" not in range "++ show bounds' ++": "++ show r
evalNodeRef env block r@(Extract d c j) = do
  val <- evalNodeRef env block d
  case val of
    Tagged c' args ->
      if | c /= c' -> Left $ "tag mismatch: "++ show (r, val)
         | j >= length args -> Left $ "OOB: "++ show (r, val)
         | otherwise -> return $ args !! j
    _ -> Left $ show r ++" where "++ show d ++" = "++ show val
evalNodeRef env block (Data c args _) = do
  js <- evalNodeRefs env block args
  return $ Tagged c js
evalNodeRef env block v | isBlockVector v = do
  v' <- sequence $ evalNodeRef env block <$> fromBlockVector v
  return $ blockVector v'
--evalNodeRef env block m
--  | isBlockMatrix m, isMatrix `all` concat m', not (null m'), replicated (map length m') =
--      unsafeCatchDeep (blockMatrix m')
--  | isBlockMatrix m = Left $
--      show m ++" = "++ show m' ++" contains non-matrix blocks or is not rectangular"
--  where m' = filter (not . null) [rights [evalNodeRef env block cell | cell <- row]
--                                 | row <- fromBlockMatrix m]
evalNodeRef env block m | isBlockMatrix m = do
  m' <- sequence [sequence [evalNodeRef env block cell | cell <- row] | row <- fromBlockMatrix m]
  case m' of
    _ | isMatrix `all` concat m', not (null m'), replicated (map length m') ->
      unsafeCatchDeep (blockMatrix m')
    _ -> Left $ show m ++" = "++ show m' ++" contains non-matrix blocks or is not rectangular"
evalNodeRef env block r@(Cond cvs _) = case sequence (evalNodeRef env block <$> cs) of
  Left e -> Left $ "while evaluating condition of "++ show r ++":\n"++ indent e
  Right cs' | length (filter (1 ==) cs') /= 1 ->
              Left $ "zero or multiple true conditions in "++ show r ++" ("++ show cs' ++")\n"++
                     "where env = "++ show env
            | otherwise -> case evalNodeRef env block (fromJust . lookup 1 $ zip cs' vs) of
                Left e -> Left $ "while evaluating value of "++ show r ++":\n"++ indent e
                Right x -> Right x
  where (cs,vs) = unzip cvs
evalNodeRef env _ Unconstrained{} = Left $ "unconstrained\nwhere env = "++ show env
evalNodeRef env (Block dags) r = error . showBlock dags $ "evalNodeRef "++ show r ++"\n"++
  "where env = "++ show env

evalRange :: Env -> Block -> [(NodeRef,NodeRef)] -> [[ConstVal]]
evalRange env block sh
  | isInfinite `any` b = error "cannot eval infinite range"
  | otherwise = range (a,b)
  where (i,j) = unzip sh
        a = fromRight' . evalNodeRef env block <$> i
        b = fromRight' . evalNodeRef env block <$> j

evalShape :: Env -> Block -> [(NodeRef,NodeRef)] -> [(Integer,Integer)]
evalShape env block sh = zip a b
  where (i,j) = unzip sh
        a = integer . fromRight' . evalNodeRef env block <$> i
        b = integer . fromRight' . evalNodeRef env block <$> j

evalNode :: Env -> Block -> Node -> Eval
evalNode env block (Apply "log_det" [i] _)
  | Right m <- evalNodeRef env block i, not (isSquareMatrix m)
  = Left $ show i ++" = "++ show m ++" is not square("++ show (shape m) ++"), "++
    "cannot take log det, with env = "++ show env
evalNode env block (Apply "ifThenElse" [c,a,b] _) = do
  c' <- evalNodeRef env block c
  evalNodeRef env block $ if c' == 1 then a else b
evalNode env block (Apply "poissonProcess_lpdf" [y,_,Var i _,mean] _) = do
  y' <- evalNodeRef env block y
  let Function lam _ = lookupBlock i block
  rates <- sequence $ evalFn1 env block lam <$> toList y'
  mean' <- evalNodeRef env block mean
  return $ sum (log <$> rates) - mean'
evalNode env block (Apply "unfold" [seed] _) = do
  let lam = head [lam | LUnfold lam <- Map.keys env]
      f = evalFn1 env block lam
  r <- evalNodeRef env block seed
  f r
evalNode env block n@(Apply fn args _) = evalContext (show n) $ do
  js <- sequence (evalNodeRef env block <$> args)
  unsafeCatch $ f js
  where f = fromMaybe (error $ "builtin lookup failure: " ++ fn) $ Map.lookup fn constFuns
evalNode env block (Array sh lam _) = do
  sh' <- sequence [ do x <- evalNodeRef env block a
                       y <- evalNodeRef env block b
                       return (toInteger x, toInteger y)
                  | (a,b) <- sh ]
  ar <- sequence $ toArray
    [ toRational <$> evalFn env block lam (map fromInteger xs)
    | xs <- fromShape sh' ]
  return $ fromRationalArray ar
evalNode env block (FoldScan fs lr lam seed ls _) = do
  r  <- evalNodeRef env block seed
  xs <- evalNodeRef env block ls
  let f = evalFn2 env block lam
  case (fs,lr) of
    (Fold, Right_) -> foldrConst' f        r xs
    (Fold, Left_)  -> foldlConst' (flip f) r xs
    (Scan, Right_) -> scanrConst' f        r xs
    (Scan, Left_)  -> scanlConst' (flip f) r xs
evalNode env block (Case hd alts _) = do
  k <- evalNodeRef env block hd
  rets <- case k of
    Tagged i cs -> evalFn' env block (alts !! i) cs
    _ | typeRef hd == IntT ->
      evalFn' env block (alts !! (integer k - 1)) []
  case rets of
    [ret] -> return ret
    _ -> return $ Tagged 0 rets
evalNode env block (Unfold lam seed _) = do
  let env' = Map.insert (LUnfold lam) 0 env
      f = evalFn1 env' block lam
  r <- evalNodeRef env block seed
  f r
evalNode _ _ n@Function{} = Left $ "evalNode "++ show n

evalFn :: Env -> Block -> Lambda NodeRef -> [ConstVal] -> Eval
evalFn env block (Lambda body ret) args = head <$> evalFn' env block (Lambda body [ret]) args
evalFn' :: Env -> Block -> Lambda [NodeRef] -> [ConstVal] -> Evals
evalFn' env block (Lambda body rets) args = sequence $ evalNodeRef env'' block' <$> rets
  where block' = deriveBlock body block
        env' = Map.fromList (inputsL body `zip` args) `Map.union` env
        env'' = evalDAG block' body env' -- optional caching

evalFn1 :: Env -> Block -> Lambda NodeRef -> ConstVal -> Eval
evalFn1 env block lam a = evalFn env block lam [a]
evalFn2 :: Env -> Block -> Lambda NodeRef -> ConstVal -> ConstVal -> Eval
evalFn2 env block lam a b = evalFn env block lam [a,b]

-- TODO: deriveBlock for each dag?
evalBlock :: Block -> Env -> Env
evalBlock block@(Block dags) = compose (evalDAG block <$> reverse dags)

evalDAG :: Block -> DAG -> Env -> Env
evalDAG block dag = compose
  [ \env -> case evalNode env block node of
              Right val -> Map.insert (LVar (Internal level ptr)) val env
              Left e -> {-trace ("evalDAG: "++ e)-} env
  | (ptr,node) <- nodes dag ]
  where level = dagLevel dag

evalRec :: (Functor f, Constructor (f (FixE f))) => FixE f -> Fix f
evalRec (FixE e) = Fix $ fmap evalRec c
  where c = toConcreteC . fromRight' $ eval_ e

unifyTuple :: (ExprTuple t) => Block -> [NodeRef] -> t
           -> Map LVal (ConstVal, Type) -> Map LVal ConstVal
unifyTuple block rets vals env = Map.fromList
  [ (LVar i, fromRight' $ evalD (Map.map fst env) e) | (LVar i, e) <- Map.toAscList eenv' ]
  where eenv = EEnv $ Map.map (uncurry constDExpr) env
        EEnv eenv' = solveTuple block rets vals eenv

{-
unify :: Expression t -> ConstVal -> Env -> Env
unify e c env = env `Map.union` unifyNodeRef env block ret c
  where (ret, block) = runExpr e

unifyD :: DExpr -> ConstVal -> Env -> Env
unifyD e c env = env `Map.union` unifyNodeRef env block ret c
  where (ret, block) = runDExpr e

unifyTuple :: (ExprTuple t) => Block -> [NodeRef] -> t -> Env -> Env
unifyTuple block rets vals = compose
  [ \env -> env `Map.union` unifyNodeRef env block r v
  | (r,e) <- zip rets $ fromExprTuple vals, let Right v = evalD_ e ]

unifyNodeRef :: Env -> Block -> NodeRef -> ConstVal -> Env
unifyNodeRef env block (Var i@Internal{} _) val =
    unifyNode env block (lookupBlock i block) val
unifyNodeRef _ _ (Var ref _) val = Map.singleton (LVar ref) val
unifyNodeRef _ _ (Const c _) val = if c == val then emptyEnv else error $ show c ++" /= "++ show val
unifyNodeRef _ _ Index{} _ = trace "WARN not unifying Index" emptyEnv

unifyNode :: Env -> Block -> Node -> ConstVal -> Env
unifyNode env block (Array sh (Lambda body hd) _) val = Map.unions $ do -- TODO unify sh
    idx <- range (bounds val)
    let env' = Map.fromList (inputsL body `zip` map fromInteger idx) `Map.union` env
    return $ unifyNodeRef env' (deriveBlock body block) hd (val ! idx)
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isRight c' =
    unifyNodeRef env block (if toBool (fromRight' c') then a else b) val
  where c' = evalNodeRef env block c
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isRight a' && isRight b' =
    if fromRight' a' == fromRight' b' then emptyEnv
    else if fromRight' a' == val then unifyNodeRef env block c true
    else if fromRight' b' == val then unifyNodeRef env block c false
    else error "unification error in ifThenElse"
  where a' = evalNodeRef env block a
        b' = evalNodeRef env block b
unifyNode env block (Apply "+" [a,b] _) val | isRight a' =
    unifyNodeRef env block b (val - fromRight' a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "*" [a,b] _) val | isRight a' =
    unifyNodeRef env block b (val / fromRight' a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "/" [a,b] _) val | isRight b' =
    unifyNodeRef env block a (val * fromRight' b')
  where b' = evalNodeRef env block b
unifyNode env block (Apply "#>" [a,b] _) val | isRight a' =
    unifyNodeRef env block b ((fromRight' a') <\> val)
  where a' = evalNodeRef env block a
unifyNode env block (Apply "<>" [a,b] _) val | isRight b' =
    unifyNodeRef env block a (val <> inv (fromRight' b'))
  where b' = evalNodeRef env block b
unifyNode env block (Apply "inv" [a] _) val =
    unifyNodeRef env block a (inv val)
unifyNode env block (Apply "quad_form_diag" [m,v] _) val
  | (ArrayT (Just "corr_matrix") _ _) <- typeRef m
  = unifyNodeRef env block m m' `Map.union` unifyNodeRef env block v v'
  where m' = fromList [fromList [(val![i,j]) / ((v'![i]) * (v'![j]))
                                | j <- [lo..hi]] | i <- [lo..hi]]
        v' = fromList [sqrt (val![i,i]) | i <- [lo..hi]]
        (lo,hi):_ = shape val
unifyNode env block (Apply "deleteIndex" [a,i] _) val | isRight a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx)
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromRight' a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
unifyNode env block (Apply "insertIndex" [a,i,e] _) val | isRight a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx) `Map.union` unifyNodeRef env block e elt
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromRight' a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
        elt = val![idx]
unifyNode env block node val | isRight lhs && fromRight' lhs == val = emptyEnv
  where lhs = evalNode env block node
unifyNode env block (FoldScan Scan Left_ (Lambda dag ret) seed ls _) val =
  unifyNodeRef env block seed seed' `Map.union` unifyNodeRef env block ls ls'
  where seed' = val![1]
        ls' = fromList $ pairWith f' (elems' val)
        block' = deriveBlock dag block
        [i,j] = inputsL dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ unifyNodeRef env' block' ret y
unifyNode env block (Case hd alts _) val
  | typeRef hd == IntT, isRight `all` vals, length js == 1 =
    unifyNodeRef env block hd j
  where vals = [evalNodeRef env (deriveBlock body block) ret
               | Lambda body [ret] <- alts]
        js = elemIndices val (fromRight' <$> vals)
        j = integer $ head js + 1
unifyNode _ _ FoldScan{} _ = trace "WARN not unifying fold/scan" emptyEnv
unifyNode _ _ node val = error $
  "unable to unify node "++ show node ++" with value "++ show val
-}

aggregateLVals :: EEnv -> EEnv
aggregateLVals = aggregateFields . aggregateConds . aggregateConds'

aggregateFields :: EEnv -> EEnv
aggregateFields (EEnv env) = EEnv $ Map.union env' env
  where kvss = groupBy f [(k,v) | (k@LField{},v) <- Map.toAscList env]
        f a b = getId' (fst a) == getId' (fst b)
        env' = Map.fromList $ do
          kvs <- kvss
          let id = unreplicate $ mapMaybe (getId' . fst) kvs
          unless (and [c == 0 | (LField _ _ c _,_) <- kvs]) $ error "not a tuple"
          if and [i == j | ((LField _ _ _ i,_),j) <- zip kvs [0..]] then
            let val = DExpr $ do
                  js <- sequence $ fromDExpr . snd <$> kvs
                  return $ Data 0 js (TupleT $ typeRef <$> js)
            in return (LVar id, val)
          else trace ("WARN missing fields: only have "++ show (fst <$> kvs)) mzero

aggregateConds :: EEnv -> EEnv
aggregateConds (EEnv env) = EEnv $ Map.union env' env
  where kvss = groupBy f [(k,v) | (k@(LCond True _ _),v) <- Map.toAscList env]
        f (LCond _ a _,_) (LCond _ b _,_) = a == b
        env' = Map.fromList $ do
          kvs <- kvss
          let lval = unreplicate [l | (LCond _ l _,_) <- kvs]
              conds = [(c,val) | (LCond _ _ c,val) <- kvs]
          return (lval, substD emptyEEnv . condD $ conds)

aggregateConds' :: EEnv -> EEnv
aggregateConds' (EEnv env) = EEnv $ Map.union env' env
  where kvss = groupBy f [(k,v) | (k@(LCond False _ _),v) <- Map.toAscList env]
        f (LCond _ a _,_) (LCond _ b _,_) = a == b
        env' = Map.fromList $ do
          kvs <- kvss
          let lval = unreplicate [l | (LCond _ l _,_) <- kvs]
              conds = do
                kvs' <- groupBy (\a b -> snd a == snd b) kvs
                let val = unreplicate $ snd <$> kvs'
                    cs = [c | (LCond _ _ c,_) <- kvs']
                return (foldr1 (&&*) cs, val)
              conds' = do
                (i,(c,v)) <- zip [0..] conds
                let c' = foldr1 (&&*) $ notB . fst <$>
                      take i conds ++ drop (i+1) conds
                return (c &&* c',v)
          return (lval, substD emptyEEnv . condD $ conds')

solveD :: DExpr -> DExpr -> EEnv -> EEnv
solveD e val env = aggregateLVals $ env `unionEEnv` solveNodeRef env block ret val
  where (ret, block) = runDExpr e

-- | equation solver / transformation inverter
solve :: (ExprTuple t) => t -> t -> EEnv
solve a b = solveTuple block rets b emptyEEnv
  where (rets, block) = runExprs a

solveTupleD :: Block -> [NodeRef] -> [DExpr] -> EEnv -> EEnv
solveTupleD block rets vals = aggregateLVals . compose
  [ \env -> env `unionEEnv` solveNodeRef env block r e
  | (r,e) <- zip rets vals ]

solveTuple :: (ExprTuple t) => Block -> [NodeRef] -> t -> EEnv -> EEnv
solveTuple block rets = solveTupleD block rets . fromExprTuple

solveNodeRef :: EEnv -> Block -> NodeRef -> DExpr -> EEnv
solveNodeRef env block (Var i@Internal{} _) val =
  solveNode env block (lookupBlock i block) val
solveNodeRef _ _ (Var ref _) val = EEnv $ Map.singleton (LVar ref) val
solveNodeRef env block (Data c rs _) val = unionsEEnv
  [solveNodeRef env block r $ extractD val c j | (r,j) <- zip rs [0..]]
solveNodeRef env block (Index (Var i _) js) val | not (isInternal i) =
  EEnv $ Map.singleton (LSub i $ reDExpr env block <$> js) val
solveNodeRef _ _ (Extract (Var i t) c j) val | not (isInternal i) =
  EEnv $ Map.singleton (LField i t c j) val
solveNodeRef env block (Extract (Var i@Internal{} _) 0 j) val
  | Case hd alts _ <- lookupBlock i block, IntT <- typeRef hd =
  solveCase env block hd [Lambda dag (rets !! j) | Lambda dag rets <- alts] val
solveNodeRef env block ref val = EEnv $ Map.singleton (LConstr $ reDExpr env block ref ==* val) true
  --trace ("WARN assuming "++ show ref ++" unifies with "++ show val) emptyEEnv

toConstraint :: (MonadPlus m) => (LVal,DExpr) -> m DExpr
toConstraint (k,v) = case k of
  LVar i | p i ->
    let a = DExpr . return $ Var i UnknownType
    in return $ a ==* v
  LField i t c j | p i ->
    let a = DExpr . return $ Extract (Var i t) c j
    in return $ a ==* v
  LCond True l c -> do
    e <- toConstraint (l,v)
    return $ notB c ||* e
  LConstr e | p `all` Set.toList (dependsD e)
            , sizeD e < 10 -> return e -- TODO: sizeD limit is a hack
  _ -> trace ("WARN ignoring constraint " ++ show k) mzero
  where sizeD = length . nodes . topDAG . snd . runDExpr
        p = isSymbol

toConstraints :: EEnv -> [DExpr]
toConstraints (EEnv env) = Map.toAscList env >>= toConstraint

solveCase :: EEnv -> Block -> NodeRef -> [Lambda NodeRef] -> DExpr -> EEnv
solveCase env block hd alts val = solveCase' 1 env block hd envs
  where envs = [solveNodeRef env (deriveBlock dag block) ret val
               | Lambda dag ret <- alts]

solveCase' :: Int -> EEnv -> Block -> NodeRef -> [EEnv] -> EEnv
solveCase' offset env block hd envs = unionsEEnv $ do
  (k,env') <- zip [0..] envs
  let h = integer $ k + offset
      cenv = conditionEEnv True (reDExpr env block hd ==* h) $ aggregateLVals env'
      kenv = solveNodeRef env block hd h
      condk = conds !! k
      bs = filter (not . null) $ take k conds ++ drop (k+1) conds
  if null condk then return cenv else if (condk ==) `any` bs then mzero else
    return $ cenv `unionEEnv` conditionEEnv False (foldl1 (&&*) condk) kenv
  where conds = toConstraints <$> envs

firstDiffD :: Z -> Z -> DExpr -> DExpr -> Z
firstDiffD n def a b = find' p def $ vector (1...n)
  where p i = let i' = erase i in Expression $ (a!i') /=* (b!i') :: B

solveNodeRefs :: EEnv -> Block -> [(NodeRef,DExpr)] -> EEnv
solveNodeRefs _ _ [] = emptyEEnv
solveNodeRefs env block ((ref,val):rvs) =
  env' `unionEEnv` solveNodeRefs (unionEEnv env' env) block rvs
  where env' = solveNodeRef env block ref val

solveNode :: EEnv -> Block -> Node -> DExpr -> EEnv
solveNode env block (Array sh (Lambda body hd) t) val = EEnv . Map.fromList $ do
  kvs <- groupBy f $ Map.toAscList subs
  let i = unreplicate $ mapMaybe (getId' . fst) kvs
      e = DExpr . return $ PartiallyConstrained (zip lo' hi') (inputsT body)
            [(k,v) | (LSub _ k,v) <- kvs] t
  return (LVar i, e)
  where (lo,hi) = unzip sh
        lo' = reDExpr env block <$> lo
        hi' = reDExpr env block <$> hi
        elt = Prelude.foldl (!) val [DExpr . return $ Var i t | (i,t) <- inputsT body]
        EEnv subs = solveNodeRef env (deriveBlock body block) hd elt
        f (LSub i _,_) (LSub j _,_) = i == j
solveNode env block (Apply "exp" [a] _) val =
  solveNodeRef env block a (log val)
solveNode env block (Apply "log" [a] _) val =
  solveNodeRef env block a (exp val)
solveNode env block (Apply "negate" [a] _) val =
  solveNodeRef env block a (negate val)
solveNode env block (Apply "+" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (val - reDExpr env block a)
solveNode env block (Apply "+" [a,b] _) val | evaluable env block b =
  solveNodeRef env block a (val - reDExpr env block b)
solveNode env block (Apply "-" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (reDExpr env block a - val)
solveNode env block (Apply "-" [a,b] _) val | evaluable env block b =
  solveNodeRef env block a (val + reDExpr env block b)
solveNode env block (Apply "*" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (val / reDExpr env block a)
solveNode env block (Apply "*" [a,b] _) val | evaluable env block b =
  solveNodeRef env block a (val / reDExpr env block b)
solveNode env block (Apply "*" [a,b] _) val =
  trace ("WARN assuming "++ show a ++" * "++ show b ++" = "++ "[...]" {-show val-}) emptyEEnv
solveNode env block (Apply "/" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (reDExpr env block a / val)
solveNode env block (Apply "/" [a,b] _) val | evaluable env block b =
  solveNodeRef env block a (reDExpr env block b * val)
solveNode env block (Apply "**" [a,b] _) val =
  trace ("WARN assuming "++ show a ++" ** "++ show b ++" = "++ "[...]" {-show val-}) emptyEEnv
solveNode env block (Apply "#>" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (reDExpr env block a <\> val)
solveNode env block (Apply "<>" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (reDExpr env block a <\> val)
solveNode env block (Apply "<>" [a,b] _) val | evaluable env block b =
  solveNodeRef env block a (tr' $ tr' (reDExpr env block b) <\> tr' val)
solveNode env block (Apply "<\\>" [a,b] _) val | evaluable env block a =
  case typeRef b of
    ArrayT _ [_] _   -> solveNodeRef env block b (reDExpr env block a #> val)
    ArrayT _ [_,_] _ -> solveNodeRef env block b (reDExpr env block a <> val)
solveNode env block (Apply "quad_form_diag" [m,v] _) val
  | (ArrayT (Just "corr_matrix") _ _) <- typeRef m
  = solveNodeRef env block m m' `unionEEnv` solveNodeRef env block v v'
  where m' = matrix [ (val!i!j) / ((v'!i) * (v'!j)) | i <- 1...n, j <- 1...n ]
        v' = vector [ sqrt (val!i!i) | i <- 1...n ]
        n = vectorSize val
solveNode env block (Apply "==" [a,b] _) val = EEnv $
  Map.singleton (LConstr $ ifB val (a' ==* b') (a' /=* b')) true
  where a' = reDExpr env block a
        b' = reDExpr env block b
solveNode env block (Apply "ifThenElse" [c,a,b] _) val | evaluable env block c =
  conditionEEnv True c' (solveNodeRef env block a val) `unionEEnv`
  conditionEEnv True (notB c') (solveNodeRef env block b val)
  where c' = reDExpr env block c
solveNode env block (Apply "ifThenElse" [c,Data 0 as _,Data 1 bs _] _) val =
  solveNodeRefs env block $ (c,c') : zip as as' ++ zip bs bs'
  where c' = caseD val [const [true], const [false]]
        as' = [caseD val [const [extractD val 0 i], const [unconstrained t]]
              | (i,a) <- zip [0..] as, let t = typeRef a]
        bs' = [caseD val [const [unconstrained t], const [extractD val 1 i]]
              | (i,b) <- zip [0..] bs, let t = typeRef b]
solveNode env block (Apply "ifThenElse" [c,e1,e0] _) val = solveCase' 0 env block c envs
  where envs = [solveNodeRef env block e0 val, solveNodeRef env block e1 val]
solveNode env block (Apply "replaceIndex" [a,e,d] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at index "++ show e) $
  solveNodeRefs env block [(e,j), (d, val!j)]
  where a' = reDExpr env block a
        n = Expression $ vectorSize a' :: Z
        j = erase $ firstDiffD n (-1) a' val
solveNode env block (Apply "insertIndex" [a,e,d] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at inserted index "++ show e) $
  solveNodeRefs env block [(e,j), (d, val!j)]
  where a' = reDExpr env block a
        n = Expression $ vectorSize a' :: Z
        j = erase $ firstDiffD n (n+1) a' val
solveNode env block (Apply "deleteIndex" [a,e] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at deleted index "++ show e) $
  solveNodeRef env block e j
  where a' = reDExpr env block a
        n = Expression $ vectorSize val :: Z
        j = erase $ firstDiffD n (n+1) val a'
solveNode env block (FoldScan Scan Left_ (Lambda dag ret) seed ls _) val =
  solveNodeRefs env block [(seed,seed'), (ls,ls')]
  where (ArrayT _ [(lo,hi)] _) = typeRef ls
        lo' = reDExpr env block lo
        hi' = reDExpr env block hi
        seed' = val!lo'
        ls' = vector [ f' (val!k) (val!(k+1)) | k <- lo'...hi' ]
        block' = deriveBlock dag block
        [i,j] = inputs dag
        f' x y = let env' = insertEEnv j x env
                 in fromJust . lookupEEnv i $ solveNodeRef env' block' ret y
solveNode env block (FoldScan ScanRest Left_ (Lambda dag ret) seed ls _) val
  | evaluable env block seed = solveNodeRef env block ls ls'
  where (ArrayT _ [(lo,hi)] _) = typeRef ls
        lo' = reDExpr env block lo
        hi' = reDExpr env block hi
        seed' = reDExpr env block seed
        ls' = vector [ f' (ifB (k ==* lo') seed' (val!(k-1))) (val!k) | k <- lo'...hi' ]
        block' = deriveBlock dag block
        [i,j] = inputs dag
        f' x y = let env' = insertEEnv j x env
                 in fromJust . lookupEEnv i $ solveNodeRef env' block' ret y
solveNode _ _ (FoldScan Fold _ _ _ _ _) _ = trace "WARN assuming fold = val" emptyEEnv
solveNode env block (Case hd alts _) val | IntT <- typeRef hd, Lambda _ [_] <- head alts =
  solveCase env block hd [Lambda dag ret | Lambda dag [ret] <- alts] val
solveNode _ _ (Apply "findSortedInsertIndex" _ _) _ = emptyEEnv
solveNode env (Block dags) n v = error $
  "solveNode:\n"++ showBlock dags (show n) ++"\n=\n"++ show v ++"\nwith env:\n"++ show env

diff :: Env -> Expression t -> Id -> Type -> ConstVal
diff env = diffD env . erase

diff_ :: Expression t -> Id -> Type -> ConstVal
diff_ = diff emptyEnv

diffD :: Env -> DExpr -> Id -> Type -> ConstVal
diffD env e = diffNodeRef env block ret
  where (ret, block) = runDExpr e

diffD_ :: DExpr -> Id -> Type -> ConstVal
diffD_ = diffD emptyEnv

diffNodeRef :: Env -> Block -> NodeRef -> Id -> Type -> ConstVal
diffNodeRef env block (Var i@Internal{} _) var t =
  diffNode env block (lookupBlock i block) var t
diffNodeRef env block (Var i s) var t
  | i == var  = eye   (numelType env block s)
  | otherwise = zeros (numelType env block s) (numelType env block t)

diffNode :: Env -> Block -> Node -> Id -> Type -> ConstVal
diffNode env block (Apply "+" [a,b] _) var t | isRight a' =
    diffNodeRef env block b var t
  where a' = evalNodeRef (Map.filterWithKey (const . (Just var /=) . getId') env) block a
diffNode env block (Apply "#>" [a,b] _) var t | isRight a' =
    fromRight' a' <> diffNodeRef env block b var t
  where a' = evalNodeRef (Map.filterWithKey (const . (Just var /=) . getId') env) block a
diffNode _ _ node var _ = error $
  "unable to diff node "++ show node ++" wrt "++ show var

deriv :: EEnv -> Expression s -> Expression t -> RMat
deriv env e = Expression . derivNodeRef env block ret . fst . runExpr
  where (ret, block) = runExpr e

deriv_ :: Expression s -> Expression t -> RMat
deriv_ = deriv emptyEEnv

-- | automatic differentiation, eg:
--
-- > (z**2) `d` z
d :: (ExprTuple s, ExprTuple t) => s -> t -> RMat
d x y = subst emptyEEnv $ deriv_ (detuple x) (detuple y)

derivD :: EEnv -> DExpr -> NodeRef -> DExpr
derivD env e = derivNodeRef env block ret
  where (ret, block) = runDExpr e

derivD_ :: DExpr -> NodeRef -> DExpr
derivD_ = derivD emptyEEnv

independent :: NodeRef -> NodeRef -> Bool
independent (Var Internal{} _) _ = False
independent (Var i _) (Var j _)
  | not (isInternal i), not (isInternal j) = i /= j
independent (Index u _) v = independent u v
independent (Extract u c j) (Extract u' c' j')
  | independent u u' || c /= c' || j /= j' = True
independent (Extract u _ _) v = independent u v
independent u (Extract v _ _) = independent u v
independent Const{} _ = True
independent _ _ = False

eye' :: EEnv -> Block -> NodeRef -> DExpr
eye' env block v = eye (vectorSize $ reDExpr env block v)

zeros' :: EEnv -> Block -> NodeRef -> NodeRef -> DExpr
zeros' env block u v = zeros (vectorSize $ reDExpr env block u)
                             (vectorSize $ reDExpr env block v)

derivNodeRef :: EEnv -> Block -> NodeRef -> NodeRef -> DExpr
derivNodeRef _ _ u _ | typeRef u == IntT = error $ "can't differentiate integer "++ show u
derivNodeRef _ _ _ v | typeRef v == IntT = error $ "can't differentiate wrt integer "++ show v
derivNodeRef env block (Data _ is _) (Data _ js _) = blockMatrix
  [[derivNodeRef env block i j | j <- js, typeRef j /= IntT] | i <- is, typeRef i /= IntT]
derivNodeRef env block (Data _ is _) j = blockMatrix
  [[derivNodeRef env block i j] | i <- is, typeRef i /= IntT]
derivNodeRef env block i (Data _ js _) = blockMatrix
  [[derivNodeRef env block i j | j <- js, typeRef j /= IntT]]
derivNodeRef env block (Var i@Internal{} _) var =
  derivNode env block (lookupBlock i block) var
derivNodeRef env block u v | independent u v = zeros' env block u v
derivNodeRef env block u@Var{} v@Var{} | getId u == getId v = eye' env block v
derivNodeRef env block u@Extract{} v | u == v = eye' env block v
derivNodeRef env block (Index i [x]) (Index j [y]) | i == j =
  ifB (x' ==* y') (constDExpr 1 t) (constDExpr 0 t)
  where x' = reDExpr env block x
        y' = reDExpr env block y
        t = typeIndex 1 $ typeRef i
derivNodeRef env block (Index u [x]) v | u == v = asRow $
  vector [ ifB (x' ==* j) (constDExpr 1 t) (constDExpr 0 t) | j <- 1...n' ]
  where t = typeIndex 1 $ typeRef u
        n' = vectorSize $ reDExpr env block u
        x' = reDExpr env block x
derivNodeRef env block u@(Extract i 0 _) v
  | TupleT ts <- typeRef i, i == v = blockMatrix
    [[derivNodeRef env block u (Extract v 0 j) | (j,t) <- zip [0..] ts, t /= IntT]]
derivNodeRef env block (Extract (Var i@Internal{} _) 0 k) v
  | Case hd alts t <- lookupBlock i block =
    derivNode env block (Case hd [Lambda dag [rets !! k] | Lambda dag rets <- alts] t) v
derivNodeRef env block (Cond cvs _) var = condD
  [(reDExpr env block c, derivNodeRef env block x var) | (c,x) <- cvs]
derivNodeRef _ _ Unconstrained{} _ = unconstrained UnknownType
derivNodeRef _ block ref var = error . showLet' (topDAG block) $
  "d "++ show ref ++" / d "++ show var

derivNode :: EEnv -> Block -> Node -> NodeRef -> DExpr
derivNode env block (Array sh (Lambda body hd) _) var = array' Nothing (length sh)
  [ let env' = bindInputs body i `unionEEnv` env
    in derivNodeRef env' block' hd var | i <- fromShape sh' ]
  where sh' = [(reDExpr env block lo, reDExpr env block hi) | (lo,hi) <- sh]
        block' = deriveBlock body block
derivNode env block (Apply "ifThenElse" [c,a,b] _) var =
  ifB (reDExpr env block c) (derivNodeRef env block a var)
                            (derivNodeRef env block b var)
derivNode env block (Apply "exp" [a] _) var =
  exp (reDExpr env block a) * derivNodeRef env block a var
derivNode env block (Apply "log" [a] _) var =
  derivNodeRef env block a var / reDExpr env block a
derivNode env block (Apply "+" [a,b] _) var
  | evaluable env' block a = derivNodeRef env block b var
  | otherwise = derivNodeRef env block a var + derivNodeRef env block b var
  where env' = filterEEnv (const . (getId var /=) . getId') env
derivNode env block (Apply "-" [a,b] _) var =
  derivNodeRef env block a var - derivNodeRef env block b var
derivNode env block (Apply op [a,b] _) var
  | op == "*" = f' * g + f * g'
  | op == "/" = (f' * g - f * g') / (g ** 2)
  | op == "**" = f**g * (g * f' / f + g' * log f)
  where f = reDExpr env block a
        f' = derivNodeRef env block a var
        g = reDExpr env block b
        g' = derivNodeRef env block b var
derivNode env block (Apply "#>" [a,b] _) var | evaluable env' block a =
  reDExpr env block a <> derivNodeRef env block b var
  where env' = filterEEnv (const . (getId var /=) . getId') env
derivNode env block (Apply "<\\>" [a,b] _) var | evaluable env' block a =
  reDExpr env block a <\> derivNodeRef env block b var
  where env' = filterEEnv (const . (getId var /=) . getId') env
derivNode env block (Apply "deleteIndex" [a,j] _) var =
  deleteIndex (derivNodeRef env block a var) (reDExpr env block j)
derivNode env block (Apply "insertIndex" [a,j,e] _) var | isScalar (typeRef e) =
  insertIndex (derivNodeRef env block a var) (reDExpr env block j)
              (derivNodeRef env block e var ! 1)
derivNode env block (Apply "replaceIndex" [a,j,e] _) var | isScalar (typeRef e) =
  replaceIndex (derivNodeRef env block a var) (reDExpr env block j)
               (derivNodeRef env block e var ! 1)
derivNode env block (Case hd alts _) var = caseD hd' alts'
  where hd' = reDExpr env block hd
        alts' = do
          Lambda dag rets <- alts
          let block' = deriveBlock dag block
          return $ const [derivNodeRef env block' ret var | ret <- rets]
derivNode _ block node var = error . showLet' (topDAG block) $
  "d "++ show node ++" / d "++ show var

instance (ExprType t, Enum t) => Enum (Expression t)

instance (ExprType t, Real t) => Real (Expression t) where
  toRational = real . fromRight' . eval_

instance (ExprType t, Integral t) => Integral (Expression t) where
  toInteger = integer . fromRight' . eval_

instance IsList RVec where
    type Item RVec = Double
    fromList xs = expr . return $ Const c t
      where c = fromList $ map real xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] RealT
    toList = map real . toList . fromRight' . eval_

instance IsList ZVec where
    type Item ZVec = Integer
    fromList xs = expr . return $ Const c t
      where c = fromList $ map integer xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] IntT
    toList = map integer . toList . fromRight' . eval_

instance IsList BVec where
    type Item BVec = Bool
    fromList xs = expr . return $ Const c t
      where c = fromList $ map fromBool xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] boolT
    toList = map toBool . toList . fromRight' . eval_

instance IsList RMat where
    type Item RMat = [Double]
    fromList xs = expr . return $ Const c t
      where n = toInteger $ length xs
            m = toInteger . length $ xs!!1
            c = Approx $ listArray ([1,1],[n,m]) (concat xs)
            t = ArrayT Nothing [(Const 1 IntT, Const (fromInteger n) IntT)
                               ,(Const 1 IntT, Const (fromInteger m) IntT)] RealT
    toList = map (map real . toList) . toList . fromRight' . eval_

instance (ExprType t) => ToConstVal t where
  toConstVal = fromRight' . eval_ . fromConcrete

instance forall t. (Show t, ExprType t) => Show (Expression t) where
  showsPrec i x = case eval_ x of
    Right c  -> showsPrec i (toConcrete c :: t)
    Left _ -> showsPrec i (erase x)

instance (Show1 f, Functor f, Constructor (f (FixE f))) => Show (FixE f) where
  showsPrec i = showsPrec i . evalRec

wrapReadsPrec :: (Read a) => (a -> t) -> Int -> ReadS t
wrapReadsPrec f d s = [(f x, s') | (x, s') <- readsPrec d s]

instance Read R    where readsPrec = wrapReadsPrec (expr . return . flip Const RealT . fromDouble)
instance Read Z    where readsPrec = wrapReadsPrec fromInteger
instance Read B    where readsPrec = wrapReadsPrec fromBool
instance Read RVec where readsPrec = wrapReadsPrec fromList
instance Read ZVec where readsPrec = wrapReadsPrec fromList
instance Read BVec where readsPrec = wrapReadsPrec fromList
instance Read RMat where readsPrec = wrapReadsPrec fromList
