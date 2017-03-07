{-# LANGUAGE FlexibleInstances, MonadComprehensions, TypeFamilies #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>))

import Data.Array.Abstract
import Data.Boolean
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Ix
import Data.Maybe
import Debug.Trace
import GHC.Exts
import Util

type Env = [(Id, ConstVal)]
emptyEnv :: Env
emptyEnv = []

builtins :: [(String, [ConstVal] -> ConstVal)]
builtins =
  [("+", \[a,b] -> a + b)
  ,("-", \[a,b] -> a - b)
  ,("*", \[a,b] -> a * b)
  ,("/", \[a,b] -> a / b)
  ,("**", \[a,b] -> a ** b)
  ,("negate", negate . head)
  ,("exp", exp . head)
  ,("log", log . head)
  ,("sqrt", sqrt . head)
  ,("true", const true)
  ,("false", const false)
  ,("pi", const pi)
  ,("asVector", head)
  ,("asMatrix", head)
  ,("<>", \[a,b] -> a <> b)
  ,("<.>", \[u,v] -> u <.> v)
  ,("*>", \[a,v] -> a *> v)
  ,("#>", \[m,v] -> m #> v)
  ,("chol", chol . head)
  ,("inv", inv . head)
  ,("tr", tr . head)
  ,("tr'", tr' . head)
  ,("ifThenElse", \[a,b,c] -> ifB a b c)
  ,("==", \[a,b] -> a ==* b)
  ,("/=", \[a,b] -> a /=* b)
  ,("<=", \[a,b] -> a <=* b)
  ,(">=", \[a,b] -> a >=* b)
  ,("<",  \[a,b] -> a <*  b)
  ,(">",  \[a,b] -> a >*  b)
  ,("deleteIndex", \[a,i]   -> deleteIndex a [integer i])
  ,("insertIndex", \[a,i,x] -> insertIndex a [integer i] x)
  ]

-- number of elements in a value of the given type
numelType :: Env -> Block -> Type -> Integer
numelType _ _ IntT = 1
numelType _ _ RealT = 1
numelType _ _ SubrangeT{} = 1
numelType env block (ArrayT _ sh _) = product $ map f sh'
  where sh' = evalShape env block sh
        f (lo,hi) = hi - lo + 1

eval :: Env -> Expr t -> Maybe ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

eval_ :: Expr t -> Maybe ConstVal
eval_ = eval []

evalD :: Env -> DExpr -> Maybe ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalD_ :: DExpr -> Maybe ConstVal
evalD_ = evalD []

evalTuple :: (ExprTuple t) => Env -> t -> Maybe [ConstVal]
evalTuple env = sequence . map (evalD env) . fromExprTuple

evalNodeRef :: Env -> Block -> NodeRef -> Maybe ConstVal
evalNodeRef env block (Var i@(Internal level ptr) _) | lookup i env == Nothing =
  let dag = reverse block !! level
      node = fromMaybe (error $ "internal lookup failure: " ++ show i) $
        ptr `lookup` nodes dag
  in evalNode env block node
evalNodeRef env _ (Var ident _) = case lookup ident env of
  Just c -> Just c
  Nothing -> trace ("WARN lookup failed: "++ show ident) Nothing
evalNodeRef _ _ (Const c) = Just c
evalNodeRef env block (Index arr idx) = do
  a <- evalNodeRef env block arr
  js <- sequence (evalNodeRef env block <$> idx)
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
evalNode env block (Apply fn args _) = do
  let f = fromMaybe (error $ "builtin lookup failure: " ++ fn) (lookup fn builtins)
  cs <- sequence (evalNodeRef env block <$> args)
  return (f cs)
-- TODO: make array Approx if any elements are
evalNode env block (Array sh body hd _) = do
  sh' <- sequence [ do x <- evalNodeRef env block a
                       y <- evalNodeRef env block b
                       return (toInteger x, toInteger y)
                  | (a,b) <- sh ]
  let block' = body : drop (length block - dagLevel body) block
  ar <- sequence $ toArray [
    toRational <$> evalNodeRef (zip (inputs body) (map fromInteger xs) ++ env) block' hd
    | xs <- fromShape sh' ]
  return (Exact ar)
evalNode env block (Fold Right_ body hd seed ls _) = do
  r  <- evalNodeRef env block seed
  xs <- evalNodeRef env block ls
  foldrConst' f r xs
  where [i,j] = inputs body
        f a b = evalNodeRef env' (body:block) hd
          where env' = (i,a) : (j,b) : env

evalBlock :: Block -> Env -> Env
evalBlock block = compose (evalDAG block <$> reverse block)

evalDAG :: Block -> DAG -> Env -> Env
evalDAG block dag = compose [ extend ptr node | (ptr,node) <- nodes dag ]
  where level = dagLevel dag
        extend ptr node env
          | isJust val && lookup ident env == Nothing =
            (ident, fromJust val) : env
          | otherwise = env
          where val = evalNode env block node
                ident = Internal level ptr

unify :: Expr t -> ConstVal -> Env -> Env
unify e c env = env ++ unifyNodeRef env block ret c
  where (ret, block) = runExpr e

unifyD :: DExpr -> ConstVal -> Env -> Env
unifyD e c env = env ++ unifyNodeRef env block ret c
  where (ret, block) = runDExpr e

unifyTuple :: (ExprTuple t) => t -> t -> Env -> Env
unifyTuple rets vals env = flip compose env . reverse $
  [ unifyD d v | (d,e) <- zipExprTuple rets vals, let Just v = evalD_ e ]

unifyTuple' :: (ExprTuple t) => Block -> [NodeRef] -> t -> Env -> Env
unifyTuple' block rets vals env = flip compose env . reverse $
  [ \env' -> env' ++ unifyNodeRef env' block r v
  | (r,e) <- zip rets $ fromExprTuple vals, let Just v = evalD_ e ]

unifyNodeRef :: Env -> Block -> NodeRef -> ConstVal -> Env
unifyNodeRef env block (Var (Internal level ptr) _) val =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in unifyNode env block node val
unifyNodeRef _ _ (Var ref _) val = [(ref,val)]
unifyNodeRef _ _ (Const c) val = if c == val then [] else error $ show c ++" /= "++ show val
unifyNodeRef _ _ Index{} _ = trace "WARN not unifying Index" []

unifyNode :: Env -> Block -> Node -> ConstVal -> Env
unifyNode env block (Array sh body hd _) val = concat $ do -- TODO unify sh
    idx <- range (bounds val)
    let env' = zip (inputs body) (map fromInteger idx) ++ env
    return $ unifyNodeRef env' block' hd (val ! idx)
  where block' = body : drop (length block - dagLevel body) block
unifyNode env block (Apply "asVector" [v] _) val =
    unifyNodeRef env block v val
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isJust c' =
    unifyNodeRef env block (if toBool (fromJust c') then a else b) val
  where c' = evalNodeRef env block c
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val | isJust a' && isJust b' =
    if fromJust a' == fromJust b' then []
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
unifyNode env block (Apply "deleteIndex" [a,i] _) val | isJust a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx)
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromJust a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
unifyNode env block (Apply "insertIndex" [a,i,e] _) val | isJust a' && dimension val == 1 =
    unifyNodeRef env block i (integer idx) ++ unifyNodeRef env block e elt
  where a' = evalNodeRef env block a
        j = firstDiff (toList $ fromJust a') (toList val)
        ([lo],_) = bounds val
        idx = lo + integer j
        elt = val![idx]
unifyNode env block node val | isJust lhs && fromJust lhs == val = []
  where lhs = evalNode env block node
unifyNode _ _ Fold{} _ = trace "WARN not unifying Fold" []
unifyNode _ _ node val = error $
  "unable to unify node "++ show node ++" with value "++ show val

diff :: Env -> Expr t -> Id -> Type -> ConstVal
diff env = diffD env . erase

diff_ :: Expr t -> Id -> Type -> ConstVal
diff_ = diff []

diffD :: Env -> DExpr -> Id -> Type -> ConstVal
diffD env e = diffNodeRef env block ret
  where (ret, block) = runDExpr e

diffD_ :: DExpr -> Id -> Type -> ConstVal
diffD_ = diffD []

diffNodeRef :: Env -> Block -> NodeRef -> Id -> Type -> ConstVal
diffNodeRef env block (Var (Internal level ptr) _) var t =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in diffNode env block node var t
diffNodeRef env block (Var i s) var t
  | i == var  = eye   (1, numelType env block s)
  | otherwise = zeros (1, numelType env block s) (1, numelType env block t)

diffNode :: Env -> Block -> Node -> Id -> Type -> ConstVal
diffNode env block (Apply "asVector" [v] _) var t =
    diffNodeRef env block v var t
diffNode env block (Apply "+" [a,b] _) var t | isJust a' =
    diffNodeRef env block b var t
  where a' = evalNodeRef (delete var env) block a
diffNode env block (Apply "#>" [a,b] _) var t | isJust a' =
    fromJust a' <> diffNodeRef env block b var t
  where a' = evalNodeRef (delete var env) block a
diffNode _ _ node var _ = error $
  "unable to diff node "++ show node ++" wrt "++ show var

instance (Enum t) => Enum (Expr t)

instance (Real t) => Real (Expr t) where
  toRational = real . fromJust . eval_

instance (Integral t) => Integral (Expr t) where
  toInteger = integer . fromJust . eval_

instance IsList RVec where
    type Item RVec = Double
    fromList = expr . return . Const . fromList . map real
    toList = map real . toList . fromJust . eval_

instance IsList ZVec where
    type Item ZVec = Integer
    fromList = expr . return . Const . fromList . map integer
    toList = map integer . toList . fromJust . eval_

instance IsList BVec where
    type Item BVec = Bool
    fromList = expr . return . Const . fromList . map fromBool
    toList = map toBool . toList . fromJust . eval_

instance IsList RMat where
    type Item RMat = [Double]
    fromList = expr . return . Const . fromList . map (fromList . map real)
    toList = map (map real . toList) . toList . fromJust . eval_

instance Show R    where show = show . real
instance Show Z    where show = show . integer
instance Show B    where show = show . toBool
instance Show RVec where show = show . toList
instance Show ZVec where show = show . toList
instance Show BVec where show = show . toList
instance Show RMat where show = show . toList
