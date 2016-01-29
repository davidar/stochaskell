{-# LANGUAGE RebindableSyntax, MonadComprehensions #-}
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
    s <- joint vector $ (\i -> normal 0 1) <$> 1...n
    lsv <- normal 0 1
    lls <- normal (log 10) 1
    w <- joint vector $ (\i -> normal 0 1) <$> 1...n
    let cov = matrix $ m <$> 1...n <*> 1...n
          where m i j = exp (lsv - ((s!i) - (s!j))^2 / (2 * exp (2 * lls))) +
                          if i == j then 1e-6 else 0
        g = chol cov #> w
    phi <- joint vector $ (\i -> bernoulliLogit (g!i)) <$> 1...n
    return (g,phi,s)

inp = map (/10) [0..100]
dat = [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
       0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
       1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1,
       0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
       1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0]

main = do
  samples <- runStan [ g | (g,phi,s) <- prior 101, phi == fromList dat, s == fromList inp ]
  toFile def "out.png" $ do
    plot . points "data" $ zip inp [5 * ((fromRational . toRational) y - 0.5) :: Double | y <- dat]
    setColors [black `withOpacity` 0.1]
    plot . line "samples" $ map (zip inp) samples
