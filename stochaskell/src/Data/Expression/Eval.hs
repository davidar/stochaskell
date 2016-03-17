{-# LANGUAGE MonadComprehensions #-}

module Data.Expression.Eval where

import qualified Data.Array as A
import Data.Array.Abstract
import Data.Expression
import Data.Expression.Const
import Data.Maybe

type Env = [(Id, ConstVal)]

builtins :: [(String, [ConstVal] -> ConstVal)]
builtins =
  [("+", \[a,b] -> a + b)
  ,("exp", exp . head)
  ,("log", log . head)
  ,("pi", const pi)
  ,("asVector", head)
  ,("asMatrix", head)
  ,("#>", \[m,v] -> m #> v)
  ,("chol", chol . head)
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
        node = fromMaybe (error "invalid pointer") $
                          ptr `lookup` nodes dag
    in evalNode env block node
evalNodeRef env _ (Var ident _) =
    fromMaybe (error "env lookup failure") $ lookup ident env
evalNodeRef _ _ (Const c) = c
evalNodeRef env block (Index arr idx) =
    evalNodeRef env block arr ! (toInteger . evalNodeRef env block <$> idx)

evalNode :: Env -> Block -> Node -> ConstVal
evalNode env block (Apply (Builtin fn) args _) =
    let f = fromMaybe (error $ "builtin lookup failure: " ++ fn) $
                      lookup fn builtins
    in f (evalNodeRef env block <$> args)
evalNode _ _ Apply{} = error "can't apply non-builtin function"
-- TODO: make array Approx if any elements are
evalNode env block (Array _ sh body hd _) = Exact $ toArray [
    toRational $ evalNodeRef (zip (inputs body) (map fromInteger xs) ++ env) (body:block) hd
    | xs <- fromShape sh' ]
  where sh' = [(toInteger $ evalNodeRef env block a, toInteger $ evalNodeRef env block b) | (a,b) <- sh]
