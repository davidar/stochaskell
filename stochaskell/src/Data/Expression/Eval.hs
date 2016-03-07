{-# LANGUAGE MonadComprehensions #-}

module Data.Expression.Eval where

import qualified Data.Array as A
import Data.Array.Abstract
import Data.Expression
import Data.Maybe

type Env = [(Id, ConstVal)]

builtins :: [(String, [ConstVal] -> ConstVal)]
builtins =
  [("+", \[a,b] -> a + b)
  ,("exp", exp . head)
  ,("pi", const pi)
  ]

eval :: Env -> Expr t -> ConstVal
eval env e = evalNodeRef env block ret
  where (ret, block) = runExpr e

evalD :: Env -> DExpr -> ConstVal
evalD env e = evalNodeRef env block ret
  where (ret, block) = runDExpr e

evalNodeRef :: Env -> Block -> NodeRef -> ConstVal
evalNodeRef env block (Internal level ptr) =
    let dag = reverse block !! level
        node = fromMaybe (error "invalid pointer") $
                          ptr `lookup` nodes dag
    in evalNode env block node
evalNodeRef env _ (External ident _) =
    fromMaybe (error "env lookup failure") $ lookup ident env
evalNodeRef _ _ (Const c) = c
evalNodeRef env block (Index arr idx) =
    case evalNodeRef env block arr of
      (CArray ar) -> (A.!) ar (evalNodeRef env block <$> idx)
      _ -> error "can't index non-array"

evalNode :: Env -> Block -> Node -> ConstVal
evalNode env block (Apply (Id Builtin fn) args _) =
    let f = fromMaybe (error $ "builtin lookup failure: " ++ fn) $
                      lookup fn builtins
    in f (evalNodeRef env block <$> args)
evalNode _ _ Apply{} = error "can't apply non-builtin function"
evalNode env block (Array _ sh body hd _) = CArray $ toArray [
    evalNodeRef (zip (inputs body) xs ++ env) (body:block) hd
    | xs <- fromShape sh' ]
  where sh' = [(evalNodeRef env block a, evalNodeRef env block b) | (a,b) <- sh]
