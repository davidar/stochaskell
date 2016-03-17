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
import Data.List
import Data.Program (density)
import Data.Random.Distribution.Abstract
import GHC.Exts
import Language.Stan

fromBool b = if b then true else false
real = fromRational . toRational

normalChol mu cov = do
  w <- joint vector [ normal 0 1 | _ <- mu ]
  return (w, vector mu + (chol (matrix cov) #> w))

prior n = do
  s <- joint vector [ normal 0 10 | i <- 1...n ]
  lsv <- normal 0 1; lls2 <- normal (log 100) 2

  (w,g) <- normalChol [ 0 | _ <- 1...n ] [
    exp (lsv - (s!i - s!j)**2 / (2 * exp lls2))
    + if i == j then 1e-6 else 0
    | i <- 1...n, j <- 1...n ]

  phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]

  return (s, lsv, lls2, w, g, phi)

test n = do
  b <- bernoulliLogit 3
  z <- normal 0 1
  zs <- joint vector [ normal 0 10 | i <- 1...n ]
  return (b,z,zs)

main :: IO ()
main = do
  let n = 101
  (s0, lsv0, lls20, w0, g0, phi0) <- prior n
  --print $ prior n `density` (list s0, real lsv0, real lls20, list w0, list g0, list phi0)
  samples <- runStan [ g | (s,_,_,_,g, phi) <- prior n, s == list s0, phi == list phi0 ]
  toFile def "out.png" $ do
    plot $ line "truth" [sort $ zip (list s0) (list g0)]
    plot . points "data" $ zip (list s0) [if y then 2.5 else (-2.5) | y <- list phi0]
    setColors [black `withOpacity` 0.1]
    plot . line "posterior" $ map (sort . zip (list s0)) samples
