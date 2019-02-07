{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where

import Control.Monad hiding (guard)
import Data.List (sort)
import Graphics.Rendering.Chart.Easy (plot,line,points,def)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Language.Stochaskell

noise = 1e-6

kernelSE' eta ils x y =
  eta * exp (- ((x - y) * ils)^2 / 2)

prior :: R -> Z -> P (R,R,R,R,RVec,R)
prior t n = do
  eta1 <- gamma 1 1
  eta2 <- gamma 1 1
  ils1 <- gamma 1 1
  ils2 <- gamma 1 1
  s <- orderedSample n (uniform 0 t)
  c <- uniform 0 t
  return (eta1,ils1,eta2,ils2,s,c)

gpChangepoint :: (R -> R -> R) -> (R -> R -> R) -> (Z,RVec,R) -> P RVec
gpChangepoint kernel1 kernel2 (n,s,c) = do
  let mu  = vector [ 0 | _ <- 1...n ]
      cov = matrix [ let a = s!i; b = s!j in 
          (if i == j then noise else 0) +
          if a < c
          then if b < c then kernel1 a b else 0
          else if b < c then 0 else kernel2 a b
        | i <- 1...n, j <- 1...n ]
  g <- normal mu cov
  return g

main = do
  let n = 100
      s = list $ (0.1 *) <$> [1..100]
  (eta1,ils1,eta2,ils2,_,c) <- sampleP (prior 10 n)
  let k1 = kernelSE' eta1 ils1
      k2 = kernelSE' eta2 ils2
  g <- sampleP $ gpChangepoint k1 k2 (n,s,c)
  toFile def "cp_data.png" $ do
    plot $ line "data" [sort $ zip (s :: [Double]) (list g :: [Double])]
  samples <- hmcStanInit 1000 [c' | (eta1',ils1',eta2',ils2',s',c') <- prior 10 n
                                  , g' <- gpChangepoint k1 k2 (n,s',c')
                                  , s' == s, g' == g] 5
  print c
  print (last samples)
