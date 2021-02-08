{-|
Description : lower-level expression manipulation
Copyright   : (c) David A Roberts, 2015-2021
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Expression
  ( Constructor(..), ConstVal, DExpr, Eval, Expression(..)
  , EEnv, Env, Expr, ExprTuple(..), ExprType(..)
  , FixE(..), FixE', GConstructor(..)
  , Id, LVal(LVar), NodeRef
  , Tag, Tags(..), TupleSize(..), Type(..), TypeOf(..), TypesOf(..)
  , boolT, tupleT, vecT, matT
  , eval_, eval'
  , module Data.Functor.Classes
  , module Generic.Data
  , module GHC.Generics
  ) where

import Data.Expression
import Data.Expression.Const
import Data.Expression.Eval
import Data.Functor.Classes (Show1)
import Generic.Data (Generically1(..))
import GHC.Generics (Generic, Generic1)
