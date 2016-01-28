{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main where

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Data.Array.Abstract
import Data.Boolean
import Data.Expression
import Data.List
import Data.Program
import Data.Ratio
import Language.Stan

prior n = do
    s <- joint vector $ (\i -> normal 0 1) <$> 1...n
    lsv <- normal 0 1
    lls <- normal (log 10) 1
    w <- joint vector $ (\i -> normal 0 1) <$> 1...n
    let cov = matrix $ m <$> 1...n <*> 1...n
          where m i j = exp (lsv - ((s!i) - (s!j))^2 / (2 * exp (2 * lls))) + ifB (i ==* j) 1e-6 0
        g = chol cov #> w
    phi <- joint vector $ (\i -> bernoulliLogit (g!i)) <$> 1...n
    return (g,phi,s)

inp = map (%10) [0..100]
dat = [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
       0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
       1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1,
       0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
       1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0]

posterior = do
    (g,phi,s) <- prior 101
    assume phi dat
    assume s inp
    return g

main = do
  samples <- runStan posterior
  let xs = map fromRational inp :: [Double]
  toFile def "out.png" $ do
    plot . points "data" $ zip xs [5 * (fromRational y - 0.5) :: Double | y <- dat]
    setColors [black `withOpacity` 0.1]
    plot . line "samples" $ map (zip xs) samples
