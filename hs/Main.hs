module Main where

import Control.Applicative
import Data.Array.Abstract
import Data.Expression
import Data.Program

prior = do
    let n = 42
        s = vector $ (const pi) <$> 1...n
    lsv <- normal 0 1
    lls <- normal (log 10) 1
    w <- joint vector $ (\i -> normal 0 1) <$> 1...n
    let cov = matrix $ m <$> 1...n <*> 1...n
          where m i j = exp (lsv - ((s!i) - (s!j))^2 / (2 * exp (2 * lls)))
        g = chol cov #> w
    phi <- joint vector $ (\i -> bernoulliLogit (g!i)) <$> 1...n
    return phi

main = print prior
