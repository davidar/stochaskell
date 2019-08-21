{-# LANGUAGE ScopedTypeVariables #-}
module Data.Expression.Unfold where

import Control.Monad.State
import Data.Expression
import Data.Expression.Case

unfoldE :: forall t f.
  (Functor f, Constructor (f t), ExprTuple t, ExprType (f (FixE f)), ExprType (f t))
  => (t -> Expression (f t)) -> t -> FixE f
unfoldE f r = FixE (expr go) where
  go = do
    seed <- fromExpr (detuple r)
    block <- get
    d <- gets nextLevel
    let i = Dummy d 11
        s = typeRef seed
        g :: t -> FixE f
        g x = FixE $ apply "unfold" RecursiveT [detuple x]
        e :: Expression (f (FixE f))
        e = fromCase (fromConcrete . fmap g) $
              f (entuple . expr . return $ Var i s)
        slam = runLambda [(i,s)] (fromExpr e)
        TypeIs t = typeOf :: TypeOf (Expression (f (FixE f)))
    if not $ stickNode (topDAG block) (Unfold (evalState slam block) seed t)
    then liftBlock go
    else do
      lam <- slam
      hashcons $ Unfold lam seed t
