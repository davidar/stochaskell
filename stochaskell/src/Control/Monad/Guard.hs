{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Guard where

type family ConditionOf m

class (Monad m) => MonadGuard m where
    guard :: ConditionOf (m ()) -> m ()

