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

kernelCP :: Int -> RVec -> RVec -> RVec -> R -> R -> R
kernelCP n cs etas ilss x y = go 1 where
  go k | integer k == (1::Int) = if x < (cs!1) && y < (cs!1)
                            then kernelSE' (etas!1) (ilss!1) x y
                            else go 2
       | integer k == n+1 = if (cs!(k-1)) <= x && (cs!(k-1)) <= y
                            then kernelSE' (etas!k) (ilss!k) x y
                            else 0
       | otherwise        = if (cs!(k-1)) <= x && x < (cs!k)
                            && (cs!(k-1)) <= y && y < (cs!k)
                            then kernelSE' (etas!k) (ilss!k) x y
                            else go (k+1)

gpChangepoint :: Int -> R -> Z -> P (RVec,RVec,RVec,RVec,RVec)
gpChangepoint k t n = do
  etas <- joint vector [ gamma 1 1 | _ <- 1...(integer $ k+1) ]
  ilss <- joint vector [ gamma 1 1 | _ <- 1...(integer $ k+1) ]
  cs <- orderedSample (integer k) (uniform 0 t)
  s <- orderedSample n (uniform 0 t)
  let mu  = vector [ 0 | _ <- 1...n ]
      cov = matrix [ kernelCP k cs etas ilss (s!i) (s!j)
                     + if i == j then noise else 0
                   | i <- 1...n, j <- 1...n ]
  g <- normal mu cov
  return (etas,ilss,cs,s,g)

main = do
  let k = 2
      t = 10
      n = 100
  (etas,ilss,cs,s,g) <- sampleP $ gpChangepoint k t n
  toFile def "cp_data.png" $ do
    plot $ line "data" [sort $ zip (list s :: [Double]) (list g :: [Double])]
  samples <- hmcStanInit 100 [cs' | (etas',ilss',cs',s',g') <- gpChangepoint k t n
                                  , s' == s, g' == g] $ fromList [3,7]
  print cs
  print (last samples)
