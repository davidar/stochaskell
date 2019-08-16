{-|
Description : lower-level expression manipulation
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Expression
  ( Constructor(..), ConstVal, DExpr, Eval, Expression(..)
  , EEnv, Env, Expr, ExprTuple(..), ExprType(..)
  , FixE(..), GConstructor(..)
  , Id, LVal(LVar), NodeRef
  , Tag, Tags(..), TupleSize(..), Type(..), TypeOf(..), TypesOf(..)
  , boolT, tupleT, vecT, matT
  , eval_, eval'
  ) where

import Data.Expression
import Data.Expression.Const
import Data.Expression.Eval
