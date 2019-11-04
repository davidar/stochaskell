{-# LANGUAGE ScopedTypeVariables #-}
module Data.Expression.Unfold where

import Control.Monad.State
import Data.Expression
import Data.Expression.Case

foldE :: forall t f.
  (Functor f, Constructor (f (FixE f)), ExprTuple t, ExprType (f (FixE f)), ExprType (f t))
  => (Expression (f t) -> t) -> FixE' f -> t
foldE f r = entuple (expr go) where
  go = do
    seed <- fromExpr r
    block <- get
    d <- gets nextLevel
    let i = Dummy d 11
        s = typeRef seed
        g :: FixE f -> t
        g x = entuple $ apply "fold" RecursiveT [unfixE x]
        e :: Expression t
        e = detuple $ f (fromCase (fromConcrete . fmap g) . expr . return $ Var i s)
        slam = runLambda [(i,s)] (fromExpr e)
        TypeIs t = typeOf :: TypeOf t
    if not $ stickNode (topDAG block) (RecursionScheme Catamorphism (evalState slam block) seed t)
    then liftBlock go
    else do
      lam <- slam
      hashcons $ RecursionScheme Catamorphism lam seed t

unfoldE :: forall t f.
  (Functor f, Constructor (f t), ExprTuple t, ExprType (f (FixE f)), ExprType (f t))
  => (t -> Expression (f t)) -> t -> FixE' f
unfoldE f r = expr go where
  go = do
    seed <- fromExpr (detuple r)
    block <- get
    d <- gets nextLevel
    let i = Dummy d 11
        s = typeRef seed
        g :: t -> FixE f
        g x = FixE $ apply "unfold" RecursiveT [detuple x]
        e :: FixE' f
        e = fromCase (fromConcrete . fmap g) $
              f (entuple . expr . return $ Var i s)
        slam = runLambda [(i,s)] (fromExpr e)
        TypeIs t = typeOf :: TypeOf (FixE' f)
    if not $ stickNode (topDAG block) (RecursionScheme Anamorphism (evalState slam block) seed t)
    then liftBlock go
    else do
      lam <- slam
      hashcons $ RecursionScheme Anamorphism lam seed t
