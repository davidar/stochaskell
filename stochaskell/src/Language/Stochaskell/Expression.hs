module Language.Stochaskell.Expression
  ( Constructor(..), Expression(..), ExprTuple(..), ExprType(..), Tags(..), Type(..), TypeOf(..)
  , boolT, tupleT, vecT, matT
  , eval_, eval'
  ) where

import Data.Expression
import Data.Expression.Eval
