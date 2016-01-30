{-# LANGUAGE RebindableSyntax, MonadComprehensions, OverloadedLists,
             NoMonomorphismRestriction, FlexibleContexts #-}
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
import Data.Program
import GHC.Exts
import Language.Stan

prior n = do
    s <- joint vector [ normal 0 1 | i <- 1...n ]
    lsv <- normal 0 1
    lls2 <- normal (log 100) 2
    w <- joint vector [ normal 0 1 | i <- 1...n ]
    let cov = matrix [ exp (lsv - (s!i - s!j)^2 / (2 * exp lls2))
                       + if i == j then 1e-6 else 0
                     | i <- 1...n, j <- 1...n ]
        g = chol cov #> w
    phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]
    return (s, phi, g)

inp = [0,1/10..10]
dat = [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
       0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
       1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1,
       0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
       1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0]

main :: IO ()
main = do
  samples <- runStan [ g | (s, phi, g) <- prior 101, s == inp, phi == dat ]
  toFile def "out.png" $ do
    let pts = zip inp [5*y - 2.5 | y <- dat] :: [(R,R)]
    plot $ points "data" pts
    setColors [black `withOpacity` 0.1]
    plot . line "samples" $ map (zip inp) samples
