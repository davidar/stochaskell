{-|
Description : Stochaskell main module
Copyright   : (c) David A Roberts, 2015-2020
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell
  ( module Language.Stochaskell.Prelude
  -- * Expressions
  , Expression
  -- ** Array manipulation
  , Indexable(..)
  -- ** Recursion
  , foldlE, foldrE, scanlE, scanrE, foldE, unfoldE
  -- *** Helpers
  , constSymbol
  , d
  , factorial', logFactorial'
  , find', findSortedInsertIndex, findSortedInsertIndexBound
  , floor'
  , min'
  , solve, solveP
  , sum'
  , symbol, symbols

  -- * Linear algebra
  , InnerProduct(..)
  , LinearOperator(..)
  , Matrix(..)
  , Scalable(..)
  , SquareMatrix(..)
  , Transposable(..)
  , Vector(..)

  -- * Types
  , B, R, Z, BVec, RVec, ZVec, BMat, RMat, ZMat
  -- ** Type casting
  , Cast(..), boolean, integer, real, list
  -- ** Abstract arrays
  , Interval
  , AbstractArray
  , (...)

  -- * Probability distributions
  , P, Distribution, Joint(..)
  -- ** Primitives
  -- *** Boolean
  , Bernoulli, Bernoullis
  , bernoulli, bernoullis, bernoulliLogit, bernoulliLogits
  -- *** Discrete
  , Geometric, NegBinomial, PMF, Poisson
  , geometric, negBinomial, pmf, poisson
  -- *** Continuous
  , Beta, Cauchy, Gamma, InvGamma, Logistic, Normal, Normals, Uniform, Uniforms
  , beta, cauchy, gamma, invGamma, logistic, normal, normals, uniform, uniforms
  -- *** Vector
  , OrderedSample, PoissonProcess
  , orderedSample, poissonProcess
  -- *** Matrix
  , LKJ
  , corrLKJ
  -- ** Logarithmic probability density functions
  , lpdf, lpdfAux, lpdfAuxC, lpdfCond
  , pdf
  , density, density'
  -- *** Primitives
  , lpdfGamma, lpdfNegBinomial, lpdfNormal, lpdfPoisson
  , lpdfUniform, lpdfDiscreteUniform
  -- ** Transformers
  , mixture, mixture', truncated, foldP, unfoldP
  -- ** Iteration
  , chain', chainRange'
  -- ** Pre-defined probabilistic programs
  , lognormal, normalChol, normalsChol, normalCond

  -- * Sampling
  , compileCC, runCC, runStep, simulate
  -- * Inference
  , hmcStan, hmcStanInit, rjmc, rjmcC, rjmc1, rjmc1Ratio

  -- * Miscellaneous
  , binarize
  , caseZ
  , chain
  , chainRange
  , debug
  , deepseq
  , fromCase
  , fromCaseP
  , fromCaseP'
  , fromRight'
  , interpolate
  , iterateLimit
  , linspace
  , logFromLogFloat
  , loop
  , mean
  , qfDiag
  , readRealMatrix
  , selectItems
  , setRandomSeed
  , silence'
  , slice
  , square
  , staircase
  , stochaskell
  , tic
  , toc
  , vizIR
  -- ** Re-exports
  , module Data.Either.Utils
  -- *** "Data.List.Utils"
  , module Data.List.Utils
  -- *** "Data.Number.Transfinite"
  , module Data.Number.Transfinite
  -- *** "GHC.Exts"
  , module GHC.Exts
  ) where

import Prelude ()
import Control.DeepSeq
import Control.Monad.HT (iterateLimit)
import Data.Array.Abstract
import Data.Either.Utils
import Data.Expression hiding (apply,const)
import Data.Expression.Case
import Data.Expression.Const hiding (isScalar)
import Data.Expression.Const.IO
import Data.Expression.Eval
import Data.Expression.Unfold
import Data.List.Utils hiding (join)
import Data.Number.LogFloat
import Data.Number.Transfinite hiding (log,isNaN,isInfinite)
import Data.Program
import Data.Random.Distribution.Abstract
import Data.Version
import GHC.Exts (IsList(..))
import Language.CC
import Language.Graphviz
import Language.Stan
import Language.Stochaskell.Prelude
import Paths_stochaskell (version)
import System.Directory
import System.Environment
import Util

-- | perform initialisation for interactive use
stochaskell :: IO ()
stochaskell = do
  putStr $ unlines
    [ "Stochaskell, version "++ showVersion version
    , "Copyright (C) 2015-2020 David A Roberts"
    , "This program comes with ABSOLUTELY NO WARRANTY."
    , "This is free software, and you are welcome to redistribute it"
    , "under certain conditions; see the LICENSE for details."
    , ""
    , "Using installation directory at "
    ]
  pwd <- getEnv "STOCHASKELL_DIRECTORY"
  putStrLn $ "  "++ pwd
  setCurrentDirectory pwd
