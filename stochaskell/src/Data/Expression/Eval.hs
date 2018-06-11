{-# LANGUAGE FlexibleInstances, MonadComprehensions, MultiWayIf, ScopedTypeVariables, TypeFamilies #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>),isInfinite)

import Control.Exception
import Control.Monad
import Data.Array.IArray (listArray)
import Data.Array.Abstract
import Data.Boolean
import Data.Either
import Data.Either.Utils
import Data.Expression hiding (const)
import Data.Expression.Case
import Data.Expression.Const hiding (isScalar)
import Data.Expression.Extract
import Data.Ix
import Data.List
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import GHC.Exts
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
evaluable env block ref =
  deps `Set.isSubsetOf` (Set.fromList . mapMaybe getId' . Set.toList $ Map.keysSet env)
  where deps = Set.filter p $ dependsNodeRef block ref
        p Dummy{}    = True
        p Volatile{} = True
        p Internal{} = False
        p Symbol{}   = False

type Eval = Either String ConstVal
type Evals = Either String [ConstVal]

evalContext :: String -> Either String a -> Either String a
evalContext s (Left e) = Left $ s ++":\n"++ indent e
evalContext _ r = r

eval :: Env -> Expr t -> Eval
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

eval_ :: Expr t -> Eval
eval_ = eval emptyEnv

eval' :: (ScalarType t) => Expr t -> Either String t
eval' = fmap toConcrete . eval_

evalD :: Env -> DExpr -> Eval
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalD_ :: DExpr -> Eval
evalD_ = evalD emptyEnv

evalTuple :: (ExprTuple t) => Env -> t -> Evals
evalTuple env = sequence . map (evalD env) . fromExprTuple

evalEEnv_ :: EEnv -> Env
evalEEnv_ env = Map.fromList $ do
  (k,v) <- Map.toList env
  case evalD_ v of
    Left e -> trace ("evalEEnv_: "++ e) mzero
    Right x -> return (k,x)

evalNodeRefs :: Env -> Block -> [NodeRef] -> Evals
evalNodeRefs env block = sequence . map (evalNodeRef env block)

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
evalNodeRef env block m
  | isBlockMatrix m, isMatrix `all` concat m', not (null m'), replicated (map length m') =
      unsafeCatch (blockMatrix m')
  | isBlockMatrix m = Left $
      show m ++" = "++ show m' ++" contains non-matrix blocks or is not rectangular"
  where m' = filter (not . null) [rights [evalNodeRef env block cell | cell <- row]
                                 | row <- fromBlockMatrix m]
evalNodeRef env block r@(Cond cvs _) = case sequence (evalNodeRef env block <$> cs) of
  Left e -> Left $ "while evaluating condition of "++ show r ++":\n"++ indent e
  Right cs' | length (filter (1 ==) cs') /= 1 ->
              Left $ "zero or multiple true conditions in "++ show r ++" ("++ show cs' ++")"
            | otherwise -> case evalNodeRef env block (fromJust . lookup 1 $ zip cs' vs) of
                Left e -> Left $ "while evaluating value of "++ show r ++":\n"++ indent e
                Right x -> Right x
  where (cs,vs) = unzip cvs
evalNodeRef env _ Unconstrained{} = Left $ "unconstrained\nwhere env = "++ show env
evalNodeRef env (Block dags) r = error . showBlock dags $ "evalNodeRef "++ show r ++"\n"++
  "where env = "++ show env

evalRange :: Env -> Block -> [(NodeRef,NodeRef)] -> [[ConstVal]]
evalRange env block sh = range (a,b)
  where (i,j) = unzip sh
        a = fromRight . evalNodeRef env block <$> i
        b = fromRight . evalNodeRef env block <$> j

evalShape :: Env -> Block -> [(NodeRef,NodeRef)] -> [(Integer,Integer)]
evalShape env block sh = zip a b
  where (i,j) = unzip sh
        a = integer . fromRight . evalNodeRef env block <$> i
        b = integer . fromRight . evalNodeRef env block <$> j

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
  rets <- evalFn' env block (alts !! (integer k - 1)) []
  case rets of
    [ret] -> return ret
    _ -> return $ Tagged 0 rets
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

unify :: Expr t -> ConstVal -> Env -> Env
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
    unifyNodeRef env block (if toBool (fromRight c') then a else b) val
  where c' = evalNodeRef env block c
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isRight a' && isRight b' =
    if fromRight a' == fromRight b' then emptyEnv
    else if fromRight a' == val then unifyNodeRef env block c true
    else if fromRight b' == val then unifyNodeRef env block c false
    else error "unification error in ifThenElse"
  where a' = evalNodeRef env block a
        b' = evalNodeRef env block b
unifyNode env block (Apply "+" [a,b] _) val | isRight a' =
    unifyNodeRef env block b (val - fromRight a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "*" [a,b] _) val | isRight a' =
    unifyNodeRef env block b (val / fromRight a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "/" [a,b] _) val | isRight b' =
    unifyNodeRef env block a (val * fromRight b')
  where b' = evalNodeRef env block b
unifyNode env block (Apply "#>" [a,b] _) val | isRight a' =
    unifyNodeRef env block b ((fromRight a') <\> val)
  where a' = evalNodeRef env block a
unifyNode env block (Apply "<>" [a,b] _) val | isRight b' =
    unifyNodeRef env block a (val <> inv (fromRight b'))
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
        j = firstDiff (toList $ fromRight a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
unifyNode env block (Apply "insertIndex" [a,i,e] _) val | isRight a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx) `Map.union` unifyNodeRef env block e elt
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromRight a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
        elt = val![idx]
unifyNode env block node val | isRight lhs && fromRight lhs == val = emptyEnv
  where lhs = evalNode env block node
unifyNode env block (FoldScan Scan Left_ (Lambda dag ret) seed ls _) val =
  unifyNodeRef env block seed seed' `Map.union` unifyNodeRef env block ls ls'
  where seed' = val![1]
        ls' = fromList $ pairWith f' (elems' val)
        block' = deriveBlock dag block
        [i,j] = inputsL dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ unifyNodeRef env' block' ret y
unifyNode _ _ FoldScan{} _ = trace "WARN not unifying fold/scan" emptyEnv
unifyNode _ _ node val = error $
  "unable to unify node "++ show node ++" with value "++ show val

aggregateLVals :: EEnv -> EEnv
aggregateLVals = aggregateFields . aggregateConds

aggregateFields :: EEnv -> EEnv
aggregateFields env = Map.union env' env
  where kvss = groupBy f [(k,v) | (k@LField{},v) <- Map.toAscList env]
        f a b = getId' (fst a) == getId' (fst b)
        env' = Map.fromList $ do
          kvs <- kvss
          let id = unreplicate $ mapMaybe (getId' . fst) kvs
          unless (and [c == 0 | (LField _ _ c _,_) <- kvs]) $ error "not a tuple"
          if (and [i == j | ((LField _ _ _ i,_),j) <- zip kvs [0..]]) then
            let val = DExpr $ do
                  js <- sequence $ fromDExpr . snd <$> kvs
                  return $ Data 0 js (TupleT $ typeRef <$> js)
            in return (LVar id, val)
          else trace ("WARN missing fields: only have "++ show (fst <$> kvs)) mzero

aggregateConds :: EEnv -> EEnv
aggregateConds env = Map.union env' env
  where kvss = groupBy f [(k,v) | (k@LCond{},v) <- Map.toAscList env]
        f (LCond a _,_) (LCond b _,_) = a == b
        env' = Map.fromList $ do
          kvs <- kvss
          let lval = unreplicate [l | (LCond l _,_) <- kvs]
              conds = [(c,val) | (LCond _ c,val) <- kvs]
              cond' = (foldr1 (&&*) $ notB . fst <$> conds
                      ,unconstrainedLike $ snd <$> conds)
          return (lval, condD $ conds) -- ++ [cond'])

solve :: Expr t -> Expr t -> EEnv -> EEnv
solve e val = solveD (erase e) (erase val)
solveD :: DExpr -> DExpr -> EEnv -> EEnv
solveD e val env = aggregateLVals $ env `Map.union` solveNodeRef env block ret val
  where (ret, block) = runDExpr e

solveTupleD :: Block -> [NodeRef] -> [DExpr] -> EEnv -> EEnv
solveTupleD block rets vals = aggregateLVals . compose
  [ \env -> env `Map.union` solveNodeRef env block r e
  | (r,e) <- zip rets vals ]

solveTuple :: (ExprTuple t) => Block -> [NodeRef] -> t -> EEnv -> EEnv
solveTuple block rets = solveTupleD block rets . fromExprTuple

solveNodeRef :: EEnv -> Block -> NodeRef -> DExpr -> EEnv
solveNodeRef env block (Var i@Internal{} _) val =
  solveNode env block (lookupBlock i block) val
solveNodeRef _ _ (Var ref _) val = Map.singleton (LVar ref) val
solveNodeRef env block (Data c rs _) val = Map.unions
  [solveNodeRef env block r $ extractD val c j | (r,j) <- zip rs [0..]]
solveNodeRef env block (Index (Var i _) js) val | not (isInternal i) =
  Map.singleton (LSub i $ reDExpr env block <$> js) val
solveNodeRef _ _ (Extract (Var i t) c j) val | not (isInternal i) =
  Map.singleton (LField i t c j) val
solveNodeRef env block (Extract (Var i@Internal{} _) 0 j) val
  | Case hd alts _ <- lookupBlock i block, IntT <- typeRef hd =
  solveCase env block hd [Lambda dag (rets !! j) | Lambda dag rets <- alts] val
solveNodeRef env block ref val = Map.singleton (LConstr $ reDExpr env block ref ==* val) true
  --trace ("WARN assuming "++ show ref ++" unifies with "++ show val) emptyEEnv

solveCase :: EEnv -> Block -> NodeRef -> [Lambda NodeRef] -> DExpr -> EEnv
solveCase env block hd alts val = Map.unions $ do
  (k,env') <- zip [1..] envs
  let env'' = condEnv (reDExpr env block hd ==* integer k) env'
  case but (k-1) conds of
    _ | (conds !! (k-1)) == true -> return env''
    Nothing -> mzero
    Just c -> return $ env'' `Map.union` Map.fromList
      [(LCond l c, v) | (l,v) <- Map.toAscList . solveNodeRef env block hd $ integer k]
  where given :: EEnv -> [DExpr]
        given env' = do
          (k,v) <- Map.toAscList env'
          case k of
            LVar i@Symbol{} ->
              let a = DExpr . return $ Var i UnknownType
              in return $ a ==* v
            LField i@Symbol{} t c j ->
              let a = DExpr . return $ Extract (Var i t) c j
              in return $ a ==* v
            LConstr e | isSymbol `all` Set.toList (dependsD e)
                      , sizeD e < 3 -> return e -- TODO: sizeD limit is a hack
            _ -> trace ("WARN ignoring constraint" {- ++ show k-}) mzero
        sizeD = length . nodes . topDAG . snd . runDExpr
        but k bs = case filter (true /=) $ take k bs ++ drop (k+1) bs of
          [] -> Just (bs !! k)
          bs' | ((bs !! k) ==) `any` bs' -> Nothing
              | otherwise -> Just $ (bs !! k) &&* List.foldl1 (&&*) (notB <$> bs')
        (conds,envs) = unzip $ do
          Lambda dag ret <- alts
          let block' = deriveBlock dag block
              env' = solveNodeRef env block' ret val
          case given env' of
            [] -> return (true, env')
            cs -> return (List.foldl1 (&&*) cs, env')
        condEnv = Map.mapKeys . flip LCond

firstDiffD :: Z -> Z -> DExpr -> DExpr -> Z
firstDiffD n def a b = find' p def $ vector (1...n)
  where p i = let i' = erase i in Expr $ (a!i') /=* (b!i') :: B

solveNodeRefs :: EEnv -> Block -> [(NodeRef,DExpr)] -> EEnv
solveNodeRefs _ _ [] = Map.empty
solveNodeRefs env block ((ref,val):rvs) =
  env' `Map.union` solveNodeRefs (Map.union env' env) block rvs
  where env' = solveNodeRef env block ref val

solveNode :: EEnv -> Block -> Node -> DExpr -> EEnv
solveNode env block (Array sh (Lambda body hd) t) val = Map.fromList $ do
  kvs <- groupBy f $ Map.toAscList subs
  let i = unreplicate $ mapMaybe (getId' . fst) kvs
      e = DExpr . return $ PartiallyConstrained (zip lo' hi') (inputsT body)
            [(k,v) | (LSub _ k,v) <- kvs] t
  return (LVar i, e)
  where (lo,hi) = unzip sh
        lo' = reDExpr env block <$> lo
        hi' = reDExpr env block <$> hi
        elt = Prelude.foldl (!) val [DExpr . return $ Var i t | (i,t) <- inputsT body]
        subs = solveNodeRef env (deriveBlock body block) hd elt
        f (LSub i _,_) (LSub j _,_) = i == j
solveNode env block (Apply "exp" [a] _) val =
  solveNodeRef env block a (log val)
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
  trace ("WARN assuming "++ show a ++" * "++ show b ++" = "++ "[...]" {-show val-}) Map.empty
solveNode env block (Apply "**" [a,b] _) val =
  trace ("WARN assuming "++ show a ++" ** "++ show b ++" = "++ "[...]" {-show val-}) Map.empty
solveNode env block (Apply "ifThenElse" [c,(Data 0 as _),(Data 1 bs _)] _) val =
  solveNodeRefs env block $ (c,c') : zip as as' ++ zip bs bs'
  where c' = caseD val [const [true], const [false]]
        as' = [caseD val [const [extractD val 0 i], const [unconstrained t]]
              | (i,a) <- zip [0..] as, let t = typeRef a]
        bs' = [caseD val [const [unconstrained t], const [extractD val 1 i]]
              | (i,b) <- zip [0..] bs, let t = typeRef b]
solveNode env block (Apply "replaceIndex" [a,e,d] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at index "++ show e) $
  solveNodeRefs env block [(e,j), (d, val!j)]
  where a' = reDExpr env block a
        n = Expr $ vectorSize a' :: Z
        j = erase $ firstDiffD n (-1) a' val
solveNode env block (Apply "insertIndex" [a,e,d] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at inserted index "++ show e) $
  solveNodeRefs env block [(e,j), (d, val!j)]
  where a' = reDExpr env block a
        n = Expr $ vectorSize a' :: Z
        j = erase $ firstDiffD n (n+1) a' val
solveNode env block (Apply "deleteIndex" [a,e] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at deleted index "++ show e) $
  solveNodeRef env block e j
  where a' = reDExpr env block a
        n = Expr $ vectorSize val :: Z
        j = erase $ firstDiffD n (n+1) val a'
solveNode env block (FoldScan Scan Left_ (Lambda dag ret) seed ls _) val =
  solveNodeRefs env block [(seed,seed'), (ls,ls')]
  where (ArrayT _ [(lo,hi)] _) = typeRef ls
        lo' = reDExpr env block lo
        hi' = reDExpr env block hi
        seed' = val!lo'
        ls' = vector [ f' (val!k) (val!(k+1)) | k <- lo'...hi' ]
        block' = deriveBlock dag block
        [i,j] = inputsL dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ solveNodeRef env' block' ret y
solveNode env block (FoldScan ScanRest Left_ (Lambda dag ret) seed ls _) val
  | evaluable env block seed = solveNodeRef env block ls ls'
  where (ArrayT _ [(lo,hi)] _) = typeRef ls
        lo' = reDExpr env block lo
        hi' = reDExpr env block hi
        seed' = reDExpr env block seed
        ls' = vector [ f' (ifB (k ==* lo') seed' (val!(k-1))) (val!k) | k <- lo'...hi' ]
        block' = deriveBlock dag block
        [i,j] = inputsL dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ solveNodeRef env' block' ret y
solveNode _ _ (FoldScan Fold _ _ _ _ _) _ = trace ("WARN assuming fold = val") Map.empty
solveNode _ _ (Apply "findSortedInsertIndex" _ _) _ = Map.empty
solveNode env (Block dags) n v = error $
  "solveNode: "++ showBlock dags (show n ++" = "++ show v) ++"\nwith env = "++ show env

diff :: Env -> Expr t -> Id -> Type -> ConstVal
diff env = diffD env . erase

diff_ :: Expr t -> Id -> Type -> ConstVal
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
  where a' = evalNodeRef (Map.filterWithKey (const . not . (Just var ==) . getId') env) block a
diffNode env block (Apply "#>" [a,b] _) var t | isRight a' =
    fromRight a' <> diffNodeRef env block b var t
  where a' = evalNodeRef (Map.filterWithKey (const . not . (Just var ==) . getId') env) block a
diffNode _ _ node var _ = error $
  "unable to diff node "++ show node ++" wrt "++ show var

deriv :: EEnv -> Expr s -> Expr t -> RMat
deriv env e = Expr . derivNodeRef env block ret . fst . runExpr
  where (ret, block) = runExpr e

deriv_ :: Expr s -> Expr t -> RMat
deriv_ = deriv emptyEEnv

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
  [ let env' = Map.fromList (inputsL body `zip` i) `Map.union` env
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
derivNode env block (Apply "+" [a,b] _) var =
  derivNodeRef env block a var + derivNodeRef env block b var
derivNode env block (Apply "-" [a,b] _) var =
  derivNodeRef env block a var - derivNodeRef env block b var
derivNode env block (Apply op [a,b] _) var
  | op == "*" = (f' * g + f * g')
  | op == "/" = (f' * g - f * g') / (g ** 2)
  | op == "**" = f**g * (g * f' / f + g' * log f)
  where f = reDExpr env block a
        f' = derivNodeRef env block a var
        g = reDExpr env block b
        g' = derivNodeRef env block b var
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

instance (ScalarType t, Enum t) => Enum (Expr t)

instance (ScalarType t, Real t) => Real (Expr t) where
  toRational = real . fromRight . eval_

instance (ScalarType t, Integral t) => Integral (Expr t) where
  toInteger = integer . fromRight . eval_

instance IsList RVec where
    type Item RVec = Double
    fromList xs = expr . return $ Const c t
      where c = fromList $ map real xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] RealT
    toList = map real . toList . fromRight . eval_

instance IsList ZVec where
    type Item ZVec = Integer
    fromList xs = expr . return $ Const c t
      where c = fromList $ map integer xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] IntT
    toList = map integer . toList . fromRight . eval_

instance IsList BVec where
    type Item BVec = Bool
    fromList xs = expr . return $ Const c t
      where c = fromList $ map fromBool xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] boolT
    toList = map toBool . toList . fromRight . eval_

instance IsList RMat where
    type Item RMat = [Double]
    fromList xs = expr . return $ Const c t
      where n = toInteger $ length xs
            m = toInteger . length $ xs!!1
            c = Approx $ listArray ([1,1],[n,m]) (concat xs)
            t = ArrayT Nothing [(Const 1 IntT, Const (fromInteger n) IntT)
                               ,(Const 1 IntT, Const (fromInteger m) IntT)] RealT
    toList = map (map real . toList) . toList . fromRight . eval_

instance forall t. (Show t, ScalarType t) => Show (Expr t) where
  show x = case eval_ x of
    Right c  -> show (toConcrete c :: t)
    Left _ -> show (erase x)

wrapReadsPrec :: (Read a) => (a -> t) -> Int -> ReadS t
wrapReadsPrec f d s = [(f x, s') | (x, s') <- readsPrec d s]

instance Read R    where readsPrec = wrapReadsPrec (expr . return . flip Const RealT . fromDouble)
instance Read Z    where readsPrec = wrapReadsPrec fromInteger
instance Read B    where readsPrec = wrapReadsPrec fromBool
instance Read RVec where readsPrec = wrapReadsPrec fromList
instance Read ZVec where readsPrec = wrapReadsPrec fromList
instance Read BVec where readsPrec = wrapReadsPrec fromList
instance Read RMat where readsPrec = wrapReadsPrec fromList
