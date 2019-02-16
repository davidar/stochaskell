{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Guard
  ( ConditionOf
  , MonadGuard(guard)
  ) where

import qualified Control.Monad

type family ConditionOf m
class (Monad m) => MonadGuard m where
  -- | overloaded generalisation of 'Control.Monad.guard'
  guard :: ConditionOf (m ()) -> m ()

type instance ConditionOf [()] = Bool
instance MonadGuard [] where
  guard = Control.Monad.guard
