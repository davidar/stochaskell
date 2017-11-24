{-# LANGUAGE FlexibleInstances, MonadComprehensions, TypeFamilies #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>),isInfinite)

import Control.Monad.State
import Data.Array.IArray (listArray)
import Data.Array.Abstract
import Data.Boolean
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Ix
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import GHC.Exts
import Util

type Env = Map Id ConstVal
emptyEnv :: Env
emptyEnv = Map.empty

-- number of elements in a value of the given type
numelType :: Env -> Block -> Type -> Integer
numelType _ _ IntT = 1
numelType _ _ RealT = 1
numelType _ _ SubrangeT{} = 1
numelType env block (ArrayT _ sh _) = product $ map f sh'
  where sh' = evalShape env block sh
        f (lo,hi) = hi - lo + 1

reDExpr :: EEnv -> Block -> NodeRef -> DExpr
reDExpr env block = DExpr . extractNodeRef env block

evaluable :: EEnv -> Block -> NodeRef -> Bool
evaluable env block ref = deps `Set.isSubsetOf` Map.keysSet env
  where deps = Set.filter (not . isInternal) $ dependsNodeRef block ref

eval :: Env -> Expr t -> Maybe ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

eval_ :: Expr t -> Maybe ConstVal
eval_ = eval emptyEnv

evalD :: Env -> DExpr -> Maybe ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalD_ :: DExpr -> Maybe ConstVal
evalD_ = evalD emptyEnv

evalTuple :: (ExprTuple t) => Env -> t -> Maybe [ConstVal]
evalTuple env = sequence . map (evalD env) . fromExprTuple

evalNodeRef :: Env -> Block -> NodeRef -> Maybe ConstVal
evalNodeRef env _ (Var ident _) | isJust val = val
  where val = Map.lookup ident env
evalNodeRef env block (Var i@Internal{} _) =
  evalNode env block $ lookupBlock i block
evalNodeRef _ _ (Var _ _) = Nothing
evalNodeRef _ _ (Const c _) = Just c
evalNodeRef env block (Index arr idx) = do
  a <- evalNodeRef env block arr
  js <- sequence (evalNodeRef env block <$> reverse idx)
  return (a!(toInteger <$> js))

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
evalNode env block (Array sh body hd _) = do
  sh' <- sequence [ do x <- evalNodeRef env block a
                       y <- evalNodeRef env block b
                       return (toInteger x, toInteger y)
                  | (a,b) <- sh ]
  let block' = deriveBlock body block
  ar <- sequence $ toArray [
    let env' = evalDAG block' body $
          Map.fromList (inputs body `zip` map fromInteger xs) `Map.union` env
    in toRational <$> evalNodeRef env' block' hd
    | xs <- fromShape sh' ]
  return $ fromRationalArray ar
evalNode env block (FoldScan scan lr body hd seed ls _) = do
  r  <- evalNodeRef env block seed
  xs <- evalNodeRef env block ls
  let f = evalFn2 env block body hd
  case (scan,lr) of
    (False, Right_) -> foldrConst' f        r xs
    (False, Left_)  -> foldlConst' (flip f) r xs
    (True,  Right_) -> scanrConst' f        r xs
    (True,  Left_)  -> scanlConst' (flip f) r xs

evalFn2 :: Env -> Block -> DAG -> NodeRef -> ConstVal -> ConstVal -> Maybe ConstVal
evalFn2 env block body hd a b = evalNodeRef env' (deriveBlock body block) hd
  where [i,j] = inputs body
        env' = Map.insert i a $ Map.insert j b env

-- TODO: deriveBlock for each dag?
evalBlock :: Block -> Env -> Env
evalBlock block@(Block dags) = compose (evalDAG block <$> reverse dags)

evalDAG :: Block -> DAG -> Env -> Env
evalDAG block dag = compose
  [ \env -> case evalNode env block node of
              Just val -> Map.insert (Internal level ptr) val env
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
unifyNodeRef _ _ (Var ref _) val = Map.singleton ref val
unifyNodeRef _ _ (Const c _) val = if c == val then emptyEnv else error $ show c ++" /= "++ show val
unifyNodeRef _ _ Index{} _ = trace "WARN not unifying Index" emptyEnv

unifyNode :: Env -> Block -> Node -> ConstVal -> Env
unifyNode env block (Array sh body hd _) val = Map.unions $ do -- TODO unify sh
    idx <- range (bounds val)
    let env' = Map.fromList (inputs body `zip` map fromInteger idx) `Map.union` env
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
unifyNode env block (FoldScan True Left_ dag ret seed ls _) val =
  unifyNodeRef env block seed seed' `Map.union` unifyNodeRef env block ls ls'
  where seed' = val![1]
        ls' = fromList $ pairWith f' (elems' val)
        block' = deriveBlock dag block
        [i,j] = inputs dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ unifyNodeRef env' block' ret y
unifyNode _ _ FoldScan{} _ = trace "WARN not unifying fold/scan" emptyEnv
unifyNode _ _ node val = error $
  "unable to unify node "++ show node ++" with value "++ show val

solve :: Expr t -> Expr t -> EEnv -> EEnv
solve e val env = env `Map.union` solveNodeRef env block ret (erase val)
  where (ret, block) = runExpr e

solveTupleD :: Block -> [NodeRef] -> [DExpr] -> EEnv -> EEnv
solveTupleD block rets vals = compose
  [ \env -> env `Map.union` solveNodeRef env block r e
  | (r,e) <- zip rets vals ]

solveTuple :: (ExprTuple t) => Block -> [NodeRef] -> t -> EEnv -> EEnv
solveTuple block rets vals env = solveTupleD block rets (fromExprTuple vals) env

solveNodeRef :: EEnv -> Block -> NodeRef -> DExpr -> EEnv
solveNodeRef env block (Var i@Internal{} _) val =
  solveNode env block (lookupBlock i block) val
solveNodeRef _ _ (Var ref _) val = Map.singleton ref val
solveNodeRef _ _ ref val =
  trace ("WARN assuming "++ show ref ++" unifies with "++ show val) emptyEEnv

solveNode :: EEnv -> Block -> Node -> DExpr -> EEnv
solveNode env block (Apply "+" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (val - reDExpr env block a)
solveNode env block (Apply "*" [a,b] _) val | evaluable env block a =
  solveNodeRef env block b (val / reDExpr env block a)
solveNode env block (FoldScan True Left_ dag ret seed ls _) val =
  solveNodeRef env block seed seed' `Map.union` solveNodeRef env block ls ls'
  where (ArrayT _ [(lo,hi)] _) = typeRef ls
        lo' = reDExpr env block lo
        hi' = reDExpr env block hi
        seed' = val!lo'
        ls' = vector [ f' (val!k) (val!(k+1)) | k <- lo'...hi' ]
        block' = deriveBlock dag block
        [i,j] = inputs dag
        f' x y = let env' = Map.insert j x env
                 in fromJust . Map.lookup i $ solveNodeRef env' block' ret y

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
  where a' = evalNodeRef (Map.delete var env) block a
diffNode env block (Apply "#>" [a,b] _) var t | isJust a' =
    fromJust a' <> diffNodeRef env block b var t
  where a' = evalNodeRef (Map.delete var env) block a
diffNode _ _ node var _ = error $
  "unable to diff node "++ show node ++" wrt "++ show var

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

instance Show R    where show = show . real
instance Show Z    where show = show . integer
instance Show B    where show = show . toBool
instance Show RVec where show = show . toList
instance Show ZVec where show = show . toList
instance Show BVec where show = show . toList
instance Show RMat where show = show . toList

wrapReadsPrec :: (Read a) => (a -> t) -> Int -> ReadS t
wrapReadsPrec f d s = [(f x, s') | (x, s') <- readsPrec d s]

instance Read R    where readsPrec = wrapReadsPrec (expr . return . flip Const RealT . fromDouble)
instance Read Z    where readsPrec = wrapReadsPrec fromInteger
instance Read B    where readsPrec = wrapReadsPrec fromBool
instance Read RVec where readsPrec = wrapReadsPrec fromList
instance Read ZVec where readsPrec = wrapReadsPrec fromList
instance Read BVec where readsPrec = wrapReadsPrec fromList
instance Read RMat where readsPrec = wrapReadsPrec fromList
