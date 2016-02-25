{-# LANGUAGE RebindableSyntax, MonadComprehensions, OverloadedLists,
             NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Prelude hiding ((==))

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Control.Monad.Guard
import Data.Array.Abstract
import Data.Bool.Num ()
import Data.Boolean.Overload
import Data.Expression
import Data.List
import Data.Random.Distribution.Abstract
import GHC.Exts
import Language.Stan

normalChol mu cov = do
  w <- joint vector [ normal 0 1 | _ <- mu ]
  return $ vector mu + (chol (matrix cov) #> w)

prior n = do
  s <- joint vector [ normal 0 20 | i <- 1...n ]
  lsv <- normal 0 1; lls2 <- normal (log 100) 2

  g <- normalChol [ 0 | _ <- 1...n ] [
    exp (lsv - (s!i - s!j)**2 / (2 * exp lls2))
    + if i == j then 1e-6 else 0
    | i <- 1...n, j <- 1...n ]

  phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]

  return (s, g, phi)

main :: IO ()
main = do
  let n = 101
  (s0, g0, phi0) <- prior n
  samples <- runStan [ g | (s, g, phi) <- prior n, s == asList s0, phi == asList phi0 ]
  toFile def "out.png" $ do
    plot $ line "truth" [sort $ zip (asList s0) (asList g0)]
    plot . points "data" $ zip (asList s0) [if y then 2.5 else (-2.5) | y <- asList phi0]
    setColors [black `withOpacity` 0.1]
    plot . line "posterior" $ map (sort . zip (asList s0)) samples
