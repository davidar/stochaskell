{-# LANGUAGE FlexibleContexts, MonadComprehensions #-}

module Main where

import Data.Array.Abstract
import Data.Expression
import Data.Expression.Eval
import qualified Data.Number.LogFloat as LF
import Data.Program
import Data.Random.Distribution.Abstract
import Test.HUnit

assertAlmostEqual :: (Fractional a, Ord a, Show a) => String -> a -> a -> Assertion
assertAlmostEqual s a b = assertBool s $ abs (a - b) < 1e-9

testDiff = TestCase $ assertEqual "" (eval_ a) (Just b)
  where i = Volatile 0 0
        t = ArrayT Nothing [(Const 1, Const 9)] RealT
        x = expr $ return (Var i t) :: RVec
        v = vector [ cast i     :: R | i <- 1...9 ] :: RVec
        a = matrix [ cast (i+j) :: R | i <- 1...9, j <- 1...9 ] :: RMat
        b = diff_ (v + a #> x) i t

testDensityJ = TestCase $ do
  x <- sampleP p
  let r = LF.fromLogFloat $ density p x / densityJ p' x
  assertAlmostEqual "" r 1
  where mu = vector [ 0 | _ <- 1...9 ] :: RVec
        cov = matrix [ let r = cast ((i - j)*(i - j)) in exp (-r/2)
                     | i <- 1...9, j <- 1...9 ] :: RMat
        p = normal mu cov :: P RVec
        p' = normalChol 9 mu cov :: P RVec

tests = TestList
  [ TestLabel "testDiff" testDiff
  , TestLabel "testDensityJ" testDensityJ
  ]

main = do
  runTestTT tests
  return ()
