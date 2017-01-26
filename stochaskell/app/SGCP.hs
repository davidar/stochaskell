{-# LANGUAGE FlexibleContexts, MonadComprehensions, RebindableSyntax #-}
module Main where
import Prelude hiding ((==),(/=),(<),(>),(<=),(>=),foldr)

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Data.Array.Abstract
import Data.Bool.Num ()
import Data.Boolean.Overload
import Data.Expression
import Data.List hiding (foldr)
import Data.Program
import Data.Random.Distribution.Abstract
import GHC.Exts
import Language.Stan

normalChol :: Z -> RVec -> RMat -> P RVec
normalChol n mu cov = do
  w <- joint vector [ normal 0 1 | _ <- 1...n ]
  return (mu + chol cov #> w)

normalCond :: Z -> (Expr t -> Expr t -> R) -> Expr [t] -> RVec -> Expr t -> P R
normalCond n cov s y x = normal m (sqrt v)
  where c = matrix [ cov (s!i) (s!j) | i <- 1...n, j <- 1...n ]
        k = vector [ cov (s!i) x     | i <- 1...n ]
        m = y <.> (inv c #> k)
        v = cov x x - k <.> (inv c #> k)

kernel :: R -> R -> R -> R -> R
kernel lsv lls2 a b = exp (lsv - (a - b)*(a - b) / (2 * exp lls2))
                      + if a == b then 1e-6 else 0

prior :: R -> Z -> P (RVec,R,R,RVec,BVec)
prior t n = do
  s <- joint vector [ uniform 0 t | _ <- 1...n ]
  lsv <- normal 0 1; lls2 <- normal (log 99.9) 2

  let mu  = vector [ 0 | _ <- 1...n ]
      cov = matrix [ kernel lsv lls2 (s!i) (s!j) | i <- 1...n, j <- 1...n ]
  g <- normalChol n mu cov

  phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]

  return (s, lsv, lls2, g, phi)

sgcp :: R -> P (R,R,R,Z,RVec,RVec,BVec)
sgcp t = do
  cap <- gamma 1 1
  n <- poisson (cap * t)
  (s, lsv, lls2, g, phi) <- prior t n
  return (lsv, lls2, cap, n, s, g, phi)

stepN :: R -> Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepN t minN (lsv, lls2, cap, n, s, g, phi) = do
  n' <- categorical [(1/2, n + 1)
                    ,(1/2, if n > minN then n - 1 else n)]
  x <- uniform 0 t
  z <- normalCond n (kernel lsv lls2) s g x
  let s'   = vector [ if i > n then x     else s!i   | i <- 1...n' ]
      g'   = vector [ if i > n then z     else g!i   | i <- 1...n' ]
      phi' = vector [ if i > n then false else phi!i | i <- 1...n' ]
  return (lsv, lls2, cap, n', s', g', phi')

stepS :: Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepS idx (lsv, lls2, cap, n, s, g, phi) = do
  x <- normal (s!idx) (exp (lls2/2) / 10)
  z <- normalCond n (kernel lsv lls2) s g x
  let s' = vector [ if i == idx then x else s!i | i <- 1...n ]
      g' = vector [ if i == idx then z else g!i | i <- 1...n ]
  return (lsv, lls2, cap, n, s', g', phi)

stepCap :: R -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepCap t (lsv, lls2, _, n, s, g, phi) = do
  let a = cast (1 + n) :: R
  cap' <- gamma a (1 + t)
  return (lsv, lls2, cap', n, s, g, phi)

main :: IO ()
main = do
  let t = 50
  (lsv0, lls20, cap0, n0, s0, g0, phi0) <- sampleP (sgcp t)
  toFile def "sgcp.png" $ do
    plot $ line "truth" [sort $ zip (list s0) (list g0)]
    plot . points "data" $ zip (list s0) [if y then 2.5 else (-2.5) | y <- toList phi0]
