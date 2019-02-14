module Language.Stochaskell.Expression
  ( Constructor(..), ConstVal, DExpr, Eval, Expression(..)
  , ExprTuple(..), ExprType(..)
  , Tag, Tags(..), Type(..), TypeOf(..)
  , boolT, tupleT, vecT, matT
  , eval_, eval'
  ) where

import Data.Expression
import Data.Expression.Const
import Data.Expression.Eval
