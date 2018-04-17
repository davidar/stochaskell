{-# LANGUAGE FlexibleInstances, MonadComprehensions, ScopedTypeVariables, TypeFamilies #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>),isInfinite)

import Control.Exception
import Control.Monad
import Data.Array.IArray (listArray)
import Data.Array.Abstract
import Data.Boolean
import Data.Expression hiding (const)
import Data.Expression.Const hiding (isScalar)
import Data.Ix
import Data.List
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

evaluable :: EEnv -> Block -> NodeRef -> Bool
evaluable env block ref = deps `Set.isSubsetOf` Set.map getId' (Map.keysSet env)
  where deps = Set.filter (not . isInternal) $ dependsNodeRef block ref

eval :: Env -> Expr t -> Maybe ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

eval_ :: Expr t -> Maybe ConstVal
eval_ = eval emptyEnv

eval' :: (ScalarType t) => Expr t -> Maybe t
eval' = fmap toConcrete . eval_

evalD :: Env -> DExpr -> Maybe ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalD_ :: DExpr -> Maybe ConstVal
evalD_ = evalD emptyEnv

evalTuple :: (ExprTuple t) => Env -> t -> Maybe [ConstVal]
evalTuple env = sequence . map (evalD env) . fromExprTuple

evalEEnv_ :: EEnv -> Env
evalEEnv_ = Map.mapMaybe evalD_

evalNodeRef :: Env -> Block -> NodeRef -> Maybe ConstVal
evalNodeRef env _ (Var ident _) | isJust val = val
  where val = Map.lookup (LVar ident) env
evalNodeRef env block (Var i@Internal{} _) =
  evalNode env block $ lookupBlock i block
evalNodeRef _ _ (Var _ _) = Nothing
evalNodeRef _ _ (Const c _) = Just c
evalNodeRef env block (Index arr idx) = do
  a <- evalNodeRef env block arr
  js <- sequence (evalNodeRef env block <$> reverse idx)
  return (a!(toInteger <$> js))
evalNodeRef env block (Extract d c j) = do
  Tagged c' args <- evalNodeRef env block d
  assert (c == c') . return $ args !! j
evalNodeRef env block (Data c args _) = do
  js <- sequence $ evalNodeRef env block <$> args
  return $ Tagged c js
evalNodeRef _ _ Unconstrained{} = Nothing
evalNodeRef _ _ r = error $ "evalNodeRef "++ show r

evalRange :: Env -> Block -> [(NodeRef,NodeRef)] -> [[ConstVal]]
evalRange env block sh = range (a,b)
  where (i,j) = unzip sh
        a = fromJust . evalNodeRef env block <$> i
        b = fromJust . evalNodeRef env block <$> j

evalShape :: Env -> Block -> [(NodeRef,NodeRef)] -> [(Integer,Integer)]
evalShape env block sh = zip a b
  where (i,j) = unzip sh
        a = integer . fromJust . evalNodeRef env block <$> i
        b = integer . fromJust . evalNodeRef env block <$> j

evalNode :: Env -> Block -> Node -> Maybe ConstVal
evalNode env block (Apply fn args _) =
  f <$> sequence (evalNodeRef env block <$> args)
  where f = fromMaybe (error $ "builtin lookup failure: " ++ fn) $
              Map.lookup fn constFuns
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

evalFn :: Env -> Block -> Lambda NodeRef -> [ConstVal] -> Maybe ConstVal
evalFn env block (Lambda body hd) args = evalNodeRef env'' block' hd
  where block' = deriveBlock body block
        env' = Map.fromList (inputsL body `zip` args) `Map.union` env
        env'' = evalDAG block' body env' -- optional caching

evalFn2 :: Env -> Block -> Lambda NodeRef -> ConstVal -> ConstVal -> Maybe ConstVal
evalFn2 env block lam a b = evalFn env block lam [a,b]

-- TODO: deriveBlock for each dag?
evalBlock :: Block -> Env -> Env
evalBlock block@(Block dags) = compose (evalDAG block <$> reverse dags)

evalDAG :: Block -> DAG -> Env -> Env
evalDAG block dag = compose
  [ \env -> case evalNode env block node of
              Just val -> Map.insert (LVar (Internal level ptr)) val env
              Nothing -> env
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
  | (r,e) <- zip rets $ fromExprTuple vals, let Just v = evalD_ e ]

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
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isJust c' =
    unifyNodeRef env block (if toBool (fromJust c') then a else b) val
  where c' = evalNodeRef env block c
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isJust a' && isJust b' =
    if fromJust a' == fromJust b' then emptyEnv
    else if fromJust a' == val then unifyNodeRef env block c true
    else if fromJust b' == val then unifyNodeRef env block c false
    else error "unification error in ifThenElse"
  where a' = evalNodeRef env block a
        b' = evalNodeRef env block b
unifyNode env block (Apply "+" [a,b] _) val | isJust a' =
    unifyNodeRef env block b (val - fromJust a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "*" [a,b] _) val | isJust a' =
    unifyNodeRef env block b (val / fromJust a')
  where a' = evalNodeRef env block a
unifyNode env block (Apply "/" [a,b] _) val | isJust b' =
    unifyNodeRef env block a (val * fromJust b')
  where b' = evalNodeRef env block b
unifyNode env block (Apply "#>" [a,b] _) val | isJust a' =
    unifyNodeRef env block b ((fromJust a') <\> val)
  where a' = evalNodeRef env block a
unifyNode env block (Apply "<>" [a,b] _) val | isJust b' =
    unifyNodeRef env block a (val <> inv (fromJust b'))
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
unifyNode env block (Apply "deleteIndex" [a,i] _) val | isJust a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx)
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromJust a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
unifyNode env block (Apply "insertIndex" [a,i,e] _) val | isJust a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx) `Map.union` unifyNodeRef env block e elt
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromJust a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
        elt = val![idx]
unifyNode env block node val | isJust lhs && fromJust lhs == val = emptyEnv
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

aggregateFields :: EEnv -> EEnv
aggregateFields env = Map.union env' env
  where kvss = groupBy f [(k,v) | (k@LField{},v) <- Map.toAscList env]
        f a b = getId' (fst a) == getId' (fst b)
        env' = Map.fromList $ do
          kvs <- kvss
          let id = unreplicate $ getId' . fst <$> kvs
          unless (and [c == 0 | (LField _ c _,_) <- kvs]) $ error "not a tuple"
          unless (and [i == j | ((LField _ _ i,_),j) <- zip kvs [0..]]) $ error "missing fields"
          let val = DExpr $ do
                js <- sequence $ fromDExpr . snd <$> kvs
                return $ Data 0 js (TupleT $ typeRef <$> js)
          return (LVar id, val)

solve :: Expr t -> Expr t -> EEnv -> EEnv
solve e val env = aggregateFields $ env `Map.union` solveNodeRef env block ret (erase val)
  where (ret, block) = runExpr e

solveTupleD :: Block -> [NodeRef] -> [DExpr] -> EEnv -> EEnv
solveTupleD block rets vals = aggregateFields . compose
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
solveNodeRef env block (Index (Var i@Volatile{} _) js) val =
  Map.singleton (LSub i $ reDExpr env block <$> js) val
solveNodeRef _ _ (Extract (Var i@Volatile{} t) c j) val =
  Map.singleton (LField i c j) val
solveNodeRef _ _ ref val =
  trace ("WARN assuming "++ show ref ++" unifies with "++ "[...]" {-show val-}) emptyEEnv

unconstrained :: Type -> DExpr
unconstrained = DExpr . return . Unconstrained

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
  let i = unreplicate $ getId' . fst <$> kvs
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
  where c' = caseD val [const true, const false]
        as' = [caseD val [const $ extractD val 0 i, const $ unconstrained t]
              | (i,a) <- zip [0..] as, let t = typeRef a]
        bs' = [caseD val [const $ unconstrained t, const $ extractD val 1 i]
              | (i,b) <- zip [0..] bs, let t = typeRef b]
solveNode env block (Apply "replaceIndex" [a,e,d] _) val =
  trace ("WARN assuming "++ show a ++" = "++ show val ++" except at index "++ show e) $
  solveNodeRefs env block [(e,j), (d, val!j)]
  where a' = reDExpr env block a
        n = Expr $ vectorSize a' :: Z
        j = erase $ firstDiffD n (Expr $ unconstrained IntT) a' val
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
solveNode env _ n v = error $ "solveNode "++ show env ++" "++ show n ++" "++ show v

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
  | i == var  = eye   (1, numelType env block s)
  | otherwise = zeros (1, numelType env block s) (1, numelType env block t)

diffNode :: Env -> Block -> Node -> Id -> Type -> ConstVal
diffNode env block (Apply "+" [a,b] _) var t | isJust a' =
    diffNodeRef env block b var t
  where a' = evalNodeRef (Map.filterWithKey (const . not . (var ==) . getId') env) block a
diffNode env block (Apply "#>" [a,b] _) var t | isJust a' =
    fromJust a' <> diffNodeRef env block b var t
  where a' = evalNodeRef (Map.filterWithKey (const . not . (var ==) . getId') env) block a
diffNode _ _ node var _ = error $
  "unable to diff node "++ show node ++" wrt "++ show var

derivD :: EEnv -> DExpr -> NodeRef -> DExpr
derivD env e = derivNodeRef env block ret
  where (ret, block) = runDExpr e

typeMatrix :: Type -> Type -> Type
typeMatrix s t
  | (ArrayT _ [m] _) <- s, (ArrayT _ [n] _) <- t = ArrayT (Just "matrix") [m,n] RealT
  | (ArrayT _ [m] _) <- s, isScalar t            = ArrayT (Just "matrix") [m,i] RealT
  | isScalar s, (ArrayT _ [n] _) <- t            = ArrayT (Just "matrix") [i,n] RealT
  | isScalar s, isScalar t                       = ArrayT (Just "matrix") [i,i] RealT
  where i = (Const 1 IntT, Const 1 IntT)

independent :: NodeRef -> NodeRef -> Bool
independent (Var Internal{} _) _ = False
independent (Var i@Volatile{} _) (Var j@Volatile{} _) = i /= j
independent (Index u _) v = independent u v
independent (Extract u c j) (Extract u' c' j')
  | independent u u' || c /= c' || j /= j' = True
independent (Extract u _ _) v = independent u v
independent _ _ = False

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
derivNodeRef env block u v
  | independent u v = constDExpr 0 t
  | u == v = case t of
    _ | isScalar t -> constDExpr 1 t
    ArrayT{} -> eye (1, vectorSize $ reDExpr env block v)
  where t = typeRef u `typeMatrix` typeRef v
derivNodeRef _ _ (Const _ t) v = constDExpr 0 $ t `typeMatrix` typeRef v
derivNodeRef env block (Index u [x]) (Index v [y]) | u == v =
  ifB (x' ==* y') (constDExpr 1 t) (constDExpr 0 t)
  where x' = reDExpr env block x
        y' = reDExpr env block y
        t = typeIndex 1 (typeRef u) `typeMatrix` typeIndex 1 (typeRef v)
derivNodeRef env block (Index u [x]) v | u == v = asRow $
  vector [ ifB (x' ==* j) (constDExpr 1 t) (constDExpr 0 t) | j <- 1...n' ]
  where t = typeIndex 1 $ typeRef u
        n' = vectorSize $ reDExpr env block u
        x' = reDExpr env block x
derivNodeRef _ _ (Index u js) v | independent u v =
  constDExpr 0 $ typeIndex (length js) (typeRef u) `typeMatrix` typeRef v
derivNodeRef env block i@(Extract u 0 k) v
  | TupleT ts <- typeRef u, u == v = blockMatrix
    [[derivNodeRef env block i (Extract v 0 j) | (j,t) <- zip [0..] ts, t /= IntT]]
  | TupleT ts <- typeRef u, independent u v =
    constDExpr 0 $ (ts !! k) `typeMatrix` typeRef v
derivNodeRef env block u j@(Extract v 0 k)
  | TupleT ts <- typeRef v, independent u v =
    constDExpr 0 $ typeRef u `typeMatrix` (ts !! k)
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
  where f = reDExpr env block a
        f' = derivNodeRef env block a var
        g = reDExpr env block b
        g' = derivNodeRef env block b var
derivNode env block (Apply "replaceIndex" [a,j,e] _) var =
  replaceIndex (derivNodeRef env block a var) (reDExpr env block j)
               (derivNodeRef env block e var)
derivNode _ _ node var = error $ "d "++ show node ++" / d "++ show var

instance (ScalarType t, Enum t) => Enum (Expr t)

instance (ScalarType t, Real t) => Real (Expr t) where
  toRational = real . fromJust . eval_

instance (ScalarType t, Integral t) => Integral (Expr t) where
  toInteger = integer . fromJust . eval_

instance IsList RVec where
    type Item RVec = Double
    fromList xs = expr . return $ Const c t
      where c = fromList $ map real xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] RealT
    toList = map real . toList . fromJust . eval_

instance IsList ZVec where
    type Item ZVec = Integer
    fromList xs = expr . return $ Const c t
      where c = fromList $ map integer xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] IntT
    toList = map integer . toList . fromJust . eval_

instance IsList BVec where
    type Item BVec = Bool
    fromList xs = expr . return $ Const c t
      where c = fromList $ map fromBool xs
            t = ArrayT Nothing [(Const 1 IntT, Const (fromIntegral $ length xs) IntT)] boolT
    toList = map toBool . toList . fromJust . eval_

instance IsList RMat where
    type Item RMat = [Double]
    fromList xs = expr . return $ Const c t
      where n = toInteger $ length xs
            m = toInteger . length $ xs!!1
            c = Approx $ listArray ([1,1],[n,m]) (concat xs)
            t = ArrayT Nothing [(Const 1 IntT, Const (fromInteger n) IntT)
                               ,(Const 1 IntT, Const (fromInteger m) IntT)] RealT
    toList = map (map real . toList) . toList . fromJust . eval_

instance forall t. (Show t, ScalarType t) => Show (Expr t) where
  show x = case eval_ x of
    Just c  -> show (toConcrete c :: t)
    Nothing -> show (erase x)

wrapReadsPrec :: (Read a) => (a -> t) -> Int -> ReadS t
wrapReadsPrec f d s = [(f x, s') | (x, s') <- readsPrec d s]

instance Read R    where readsPrec = wrapReadsPrec (expr . return . flip Const RealT . fromDouble)
instance Read Z    where readsPrec = wrapReadsPrec fromInteger
instance Read B    where readsPrec = wrapReadsPrec fromBool
instance Read RVec where readsPrec = wrapReadsPrec fromList
instance Read ZVec where readsPrec = wrapReadsPrec fromList
instance Read BVec where readsPrec = wrapReadsPrec fromList
instance Read RMat where readsPrec = wrapReadsPrec fromList
