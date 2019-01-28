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

gpChangepoint :: (R -> R -> R) -> (R -> R -> R) -> (Z,RVec,R) -> P RVec
gpChangepoint kernel1 kernel2 (n,s,c) = do
  let k = findSortedInsertIndex c s - 1
      mu  = vector [ 0 | _ <- 1...n ]
      cov1 = matrix [ kernel1 (s!i) (s!j)
                      + if i == j then noise else 0
                    | i <- 1...k, j <- 1...k ]
      cov2 = matrix [ kernel2 (s!(k+i)) (s!(k+j))
                      + if i == j then noise else 0
                    | i <- 1...(n-k), j <- 1...(n-k) ]
      cov = blockMatrix [[cov1, zeros k (n-k)]
                        ,[zeros (n-k) k, cov2]]
  g <- normalChol n mu cov
  return g

main = do
  let n = 100
      s = list $ (0.1 *) <$> [1..100]
      c = 5
  g <- sampleP $ gpChangepoint (kernelSE' 1 1) (kernelSE' 0.2 0.2) (n,s,c)
  print g
  toFile def "cp_data.png" $ do
    plot $ line "data" [sort $ zip (s :: [Double]) (list g :: [Double])]
