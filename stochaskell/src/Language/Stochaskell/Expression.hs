module Language.Stochaskell.Expression
  ( Constructor(..), Expr(..), ExprType(..), Tags(..), Type(..), TypeOf(..)
  , eval_, eval'
  , fromExprTuple
  , vecT
  ) where

import Data.Expression
import Data.Expression.Eval
