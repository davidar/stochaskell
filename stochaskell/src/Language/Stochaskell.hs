module Language.Stochaskell
  ( module Prelude
  , module Control.Monad.Guard
  , module Data.Boolean.Overload
  , module Data.Monoid
  , module Data.Number.Transfinite
  , module GHC.Exts

  , Expression, B, R, Z, BVec, RVec, ZVec, BMat, RMat, ZMat, P
  , boolean, integer, real, list
  , joint, vector, matrix, blockVector, blockMatrix
  , (...), (!), (#>), (*>)
  , inv, tr', outer, diag, qfDiag
  , asColumn, asRow, cast

  , bernoulli, bernoulliLogit, bernoulliLogits
  , beta, cauchy, gamma, invGamma, lognormal, normal, normals, uniform, uniforms
  , geometric, negBinomial, pmf, poisson
  , orderedSample, poissonProcess
  , mixture, mixture', truncated
  , normalChol, normalsChol, normalCond
  , corrLKJ, lpdfNormal

  , compileCC, runCC, runStep, simulate
  , debug, chain, chain', chainRange, chainRange', loop
  , hmcStan, hmcStanInit, mh, mh', mhRatio, rjmc, rjmcC

  , deleteAt, insertAt, replaceAt
  , foldl, foldr, scanl, scanr
  , find', findSortedInsertIndex, min', sum'
  , binarize, fromRight', interpolate, mean, readRealMatrix, selectItems, slice, square
  ) where

import Prelude hiding (
  (==),(/=),(<),(>),(<=),(>=),(<*),(*>),(&&),(||),
  max,min,not,foldl,foldr,scanl,scanr)
import Control.Monad.Guard
import Data.Array.Abstract
import Data.Boolean.Overload hiding (boolean) -- TODO: no infix declaration
import Data.Expression hiding (apply,const)
import Data.Expression.Case
import Data.Expression.Const hiding (isScalar)
import Data.Expression.Const.IO
import Data.Monoid
import Data.Number.Transfinite hiding (log,isNaN,isInfinite)
import Data.Program
import Data.Random.Distribution.Abstract
import GHC.Exts (IsList(..))
import Language.CC
import Language.Stan
import Util
