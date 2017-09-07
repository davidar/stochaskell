{-# LANGUAGE FlexibleInstances, MonadComprehensions, TypeFamilies #-}

module Data.Expression.Eval where

import Prelude hiding ((<*),(*>))

import Data.Array (listArray)
import Data.Array.Abstract
import Data.Boolean
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Ix
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Debug.Trace
import GHC.Exts
import Util

type Env = Map Id ConstVal
emptyEnv :: Env
emptyEnv = Map.empty

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
  ,("getExternal", head)
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
evalNodeRef env block (Var i@(Internal level ptr) _) =
  let dag = reverse block !! level
      node = fromMaybe (error $ "internal lookup failure: " ++ show i) $
        ptr `lookup` nodes dag
  in evalNode env block node
evalNodeRef _ _ (Var _ _) = Nothing
evalNodeRef _ _ (Const c _) = Just c
evalNodeRef env block (Index arr idx) = do
  a <- evalNodeRef env block arr
  js <- sequence (evalNodeRef env block <$> idx) -- TODO: should this be reversed?
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
    let env' = evalDAG block' body $
          Map.fromList (inputs body `zip` map fromInteger xs) `Map.union` env
    in toRational <$> evalNodeRef env' block' hd
    | xs <- fromShape sh' ]
  return $ fromRationalArray ar
evalNode env block (Fold Right_ body hd seed ls _) = do
  r  <- evalNodeRef env block seed
  xs <- evalNodeRef env block ls
  foldrConst' f r xs
  where [i,j] = inputs body
        f a b = evalNodeRef env' (body:block) hd
          where env' = Map.insert i a $ Map.insert j b env

evalBlock :: Block -> Env -> Env
evalBlock block = compose (evalDAG block <$> reverse block)

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

unifyTuple' :: (ExprTuple t) => Block -> [NodeRef] -> t -> Env -> Env
unifyTuple' block rets vals = compose
  [ \env -> env `Map.union` unifyNodeRef env block r v
  | (r,e) <- zip rets $ fromExprTuple vals, let Just v = evalD_ e ]

unifyNodeRef :: Env -> Block -> NodeRef -> ConstVal -> Env
unifyNodeRef env block (Var (Internal level ptr) _) val =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in unifyNode env block node val
unifyNodeRef _ _ (Var ref _) val = Map.singleton ref val
unifyNodeRef _ _ (Const c _) val = if c == val then emptyEnv else error $ show c ++" /= "++ show val
unifyNodeRef _ _ Index{} _ = trace "WARN not unifying Index" emptyEnv

unifyNode :: Env -> Block -> Node -> ConstVal -> Env
unifyNode env block (Array sh body hd _) val = Map.unions $ do -- TODO unify sh
    idx <- range (bounds val)
    let env' = Map.fromList (inputs body `zip` map fromInteger idx) `Map.union` env
    return $ unifyNodeRef env' block' hd (val ! idx)
  where block' = body : drop (length block - dagLevel body) block
unifyNode env block (Apply "asVector" [v] _) val =
    unifyNodeRef env block v val
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
unifyNode _ _ Fold{} _ = trace "WARN not unifying Fold" emptyEnv
unifyNode _ _ node val = error $
  "unable to unify node "++ show node ++" with value "++ show val

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
    fromList = expr . return . flip Const undefined . fromList . map real
    toList = map real . toList . fromJust . eval_

instance IsList ZVec where
    type Item ZVec = Integer
    fromList = expr . return . flip Const undefined . fromList . map integer
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
