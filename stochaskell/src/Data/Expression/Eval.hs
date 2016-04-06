{-# LANGUAGE MonadComprehensions #-}

module Data.Expression.Eval where

import Data.Array.Abstract
import Data.Boolean
import Data.Expression
import Data.Expression.Const
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
  ]

eval :: Env -> Expr t -> ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

evalD :: Env -> DExpr -> ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalNodeRef :: Env -> Block -> NodeRef -> ConstVal
evalNodeRef env block (Var (Internal level ptr) _) =
    let dag = reverse block !! level
        node = flip fromMaybe (ptr `lookup` nodes dag) $
          error $ "pointer "++ show level ++":"++ show ptr ++" not found"
    in evalNode env block node
evalNodeRef env _ (Var ident _) =
    fromMaybe (error "env lookup failure") $ lookup ident env
evalNodeRef _ _ (Const c) = c
evalNodeRef env block (Index arr idx) =
    evalNodeRef env block arr ! (toInteger . evalNodeRef env block <$> idx)

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
