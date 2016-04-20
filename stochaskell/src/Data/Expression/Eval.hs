{-# LANGUAGE MonadComprehensions #-}

module Data.Expression.Eval where

import Prelude hiding ((<*))

import Data.Array.Abstract
import Data.Boolean
import Data.Expression
import Data.Expression.Const
import Data.Ix
import Data.Maybe

type Env = [(Id, ConstVal)]

builtins :: [(String, [ConstVal] -> ConstVal)]
builtins =
  [("+", \[a,b] -> a + b)
  ,("-", \[a,b] -> a - b)
  ,("*", \[a,b] -> a * b)
  ,("/", \[a,b] -> a / b)
  ,("**", \[a,b] -> a ** b)
  ,("exp", exp . head)
  ,("log", log . head)
  ,("pi", const pi)
  ,("asVector", head)
  ,("asMatrix", head)
  ,("#>", \[m,v] -> m #> v)
  ,("chol", chol . head)
  ,("ifThenElse", \[a,b,c] -> ifB a b c)
  ,("==", \[a,b] -> a ==* b)
  ,("/=", \[a,b] -> a /=* b)
  ,("<=", \[a,b] -> a <=* b)
  ,(">=", \[a,b] -> a >=* b)
  ,("<",  \[a,b] -> a <*  b)
  ,(">",  \[a,b] -> a >*  b)
  ]

eval :: Env -> Expr t -> ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

evalD :: Env -> DExpr -> ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalTuple :: (ExprTuple t) => Env -> t -> [ConstVal]
evalTuple env = map (evalD env) . fromExprTuple

evalNodeRef :: Env -> Block -> NodeRef -> ConstVal
evalNodeRef env block (Var (Internal level ptr) _) =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in evalNode env block node
evalNodeRef env _ (Var ident _) =
    fromMaybe (error $ "env lookup failure for "++ show ident) $ lookup ident env
evalNodeRef _ _ (Const c) = c
evalNodeRef env block (Index arr idx) =
    evalNodeRef env block arr ! (toInteger . evalNodeRef env block <$> idx)

evalRange :: Env -> Block -> [(NodeRef,NodeRef)] -> [[ConstVal]]
evalRange env block sh = range (a,b)
  where (i,j) = unzip sh
        a = evalNodeRef env block <$> i
        b = evalNodeRef env block <$> j

evalNode :: Env -> Block -> Node -> ConstVal
evalNode env block (Apply fn args _) =
  fromMaybe (error $ "builtin lookup failure: " ++ fn) (lookup fn builtins)
            (evalNodeRef env block <$> args)
-- TODO: make array Approx if any elements are
evalNode env block (Array sh body hd _) =
  Exact $ toArray [
    toRational $ evalNodeRef (zip (inputs body) (map fromInteger xs) ++ env) block' hd
    | xs <- fromShape sh' ]
  where sh' = [(toInteger $ evalNodeRef env block a, toInteger $ evalNodeRef env block b) | (a,b) <- sh]
        block' = body : drop (length block - dagLevel body) block

unifyD :: Env -> DExpr -> ConstVal -> Env
unifyD env e = unifyNodeRef env block ret
  where (ret, block) = runDExpr e

unifyNodeRef :: Env -> Block -> NodeRef -> ConstVal -> Env
unifyNodeRef env block (Var (Internal level ptr) _) val =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in unifyNode env block node val
unifyNodeRef _ _ (Var ref _) val = [(ref,val)]
unifyNodeRef _ _ (Const c) val = if c == val then [] else error $ show c ++" /= "++ show val
unifyNodeRef env block (Index arr idx) val = [] -- TODO

unifyNode :: Env -> Block -> Node -> ConstVal -> Env
unifyNode env block (Array sh body hd _) val = concat $ do -- TODO unify sh
    idx <- range (bounds val)
    let env' = zip (inputs body) (map fromInteger idx) ++ env
    return $ unifyNodeRef env' block' hd (val ! idx)
  where block' = body : drop (length block - dagLevel body) block
unifyNode env block (Apply "asVector" [v] _) val =
    unifyNodeRef env block v val
unifyNode env block (Apply "ifThenElse" [c,a,b] _) val =
    unifyNodeRef env block (if toBool c' then a else b) val
  where c' = evalNodeRef env block c
unifyNode env block (Apply fn args _) val = error fn
