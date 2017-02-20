{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Prelude hiding ((==),(/=),(<),(>),(<=),(>=),foldr)

import Graphics.Rendering.Chart.Easy ( plot, line, points, def )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Control.Monad.Guard
import Data.Array.Abstract
import Data.Boolean.Overload -- TODO: no infix declaration
import Data.Expression
import Data.Expression.Const
import Data.List hiding (foldr)
import Data.Program
import Data.Random.Distribution.Abstract
import GHC.Exts
import Language.Stan
import Util

kernel :: R -> R -> R -> R -> R
kernel lsv lls2 a b = exp (lsv - (a - b)*(a - b) / (2 * exp lls2))
                      + if a == b then 1e-6 else 0

sgcp :: R -> P (R,R,R,Z,RVec,RVec,BVec)
sgcp t = do
  lsv <- normal 0 1
  lls2 <- normal (log 99.9) 2
  cap <- gamma 1 1
  n <- poisson (cap * t)
  s <- joint vector [ uniform 0 t | _ <- 1...n ]

  let mu  = vector [ 0 | _ <- 1...n ]
      cov = matrix [ kernel lsv lls2 (s!i) (s!j) | i <- 1...n, j <- 1...n ]
  g <- normalChol n mu cov

  phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]

  return (lsv, lls2, cap, n, s, g, phi)

stepDown :: Z -> Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepDown k n' (lsv,lls2,cap,n,s,g,phi) = do
  i <- uniform (k+1) n'
  let s'   = deleteIndex s i
      g'   = deleteIndex g i
      phi' = deleteIndex phi i
  return (lsv,lls2,cap,n',s',g',phi')

stepUp :: R -> Z -> Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepUp t k n' (lsv,lls2,cap,n,s,g,phi) = do
  x <- uniform 0 t
  z <- normalCond n (kernel lsv lls2) s g x
  let f i j = if x <= (s!i) then i else j
      i = foldr f n' $ vector [ i | i <- (k+1)...n ]
      s'   = insertIndex s i x
      g'   = insertIndex g i z
      phi' = insertIndex phi i false
  return (lsv,lls2,cap,n',s',g',phi')

stepN :: R -> Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepN t k state@(lsv,lls2,cap,n,s,g,phi) = do
  n' <- categorical [(1/2, n + 1)
                    ,(1/2, if n > k then n - 1 else n)]
  (_,_,_,_,sUp,gUp,phiUp) <- stepUp t k n' state
  (_,_,_,_,sDown,gDown,phiDown) <- stepDown k n' state
  let s'   = if n' == (n + 1) then sUp
        else if n' == (n - 1) then sDown
        else s
      g'   = if n' == (n + 1) then gUp
        else if n' == (n - 1) then gDown
        else g
      phi' = if n' == (n + 1) then phiUp
        else if n' == (n - 1) then phiDown
        else phi
  return (lsv,lls2,cap,n',s',g',phi')

stepS :: Z -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepS idx (lsv, lls2, cap, n, s, g, phi) = do
  x <- normal (s!idx) (exp (lls2/2))
  z <- normalCond n (kernel lsv lls2) s g x
  let s' = vector [ if i == idx then x else s!i | i <- 1...n ]
      g' = vector [ if i == idx then z else g!i | i <- 1...n ]
  return (lsv, lls2, cap, n, s', g', phi)

stepCap :: R -> (R,R,R,Z,RVec,RVec,BVec) -> P (R,R,R,Z,RVec,RVec,BVec)
stepCap t (lsv, lls2, _, n, s, g, phi) = do
  let a = cast (1 + n) :: R
  cap' <- gamma a (1 + t)
  return (lsv, lls2, cap', n, s, g, phi)

genData :: R -> IO [Double]
genData t = do
  (_,_,_,_,s,g,phi) <- sampleP (sgcp t)
  toFile def "sgcp_data.png" $ do
    plot $ line "truth" [sort $ zip (toList s) (toList g)]
    plot . points "data" $ zip (toList s) [if y then 2.5 else (-2.5) | y <- toList phi]
  return $ toList s `selectItems` toList phi

genData' :: Double -> IO [Double]
genData' t = do
  let cap = 2
  n <- poisson (t * cap)
  s <- sequence [ uniform 0 t | _ <- [1..n :: Integer] ]
  let f = [ 2 * exp (-x/15) + exp (-((x-25)/10)^2) | x <- s ]
  phi <- sequence [ bernoulli (y / cap) | y <- f ]
  let dat = s `selectItems` phi
  toFile def "sgcp_data.png" $ do
    plot $ line "truth" [sort $ zip s f]
    plot . points "data" $ zip dat (repeat 1.9)
  return $ sort dat

main :: IO ()
main = do
  let t = 50
  dat <- genData' t

  let k = integer (length dat)
      cap = 2 * k / t
      m = 10
      n = k + m
      phi = fromList $ replicate k True ++ replicate m False :: BVec

  rej <- sequence [ uniform 0 t | _ <- [1..m] ]
  let s = fromList $ dat ++ sort rej :: RVec

  samples <- hmcStan [ (lsv,lls2,g) | (lsv,lls2,cap',n',s',g,phi') <- sgcp t,
                       cap' == real cap, n' == integer n, s' == s, phi' == phi ]
  let (lsv,lls2,g) = last samples

  let multiplicity (_,_,_,n,_,_,_) = lfact (integer n)
      propose = mhAdjust multiplicity (sgcp t)

  loop (0,lsv,lls2,cap,n,s,g,phi) $ \(iter,lsv,lls2,cap,n,s,g,phi) -> do
    (n,s,g,phi) <- flip (chain 10) (n,s,g,phi) $ \(n,s,g,phi) -> do
      -- propose trans-dimensional jump
      (_,_,_,n,s,g,phi) <- propose (stepN t k) (lsv,lls2,cap,n,s,g,phi)
      return (n,s,g,phi)

    let m = integer n - k
    (_,s,g) <- flip (chain m) (k+1,s,g) $ \(i,s,g) -> do
      -- propose move for s!i
      (_,_,_,_,s,g,_)  <- propose (stepS (integer i)) (lsv,lls2,cap,n,s,g,phi)
      return (i+1,s,g)

    -- update cap
    (_,_,cap,_,_,_,_) <- propose (stepCap t) (lsv,lls2,cap,n,s,g,phi)

    -- fit GP via Stan
    samples <- hmcStanInit' 10 [ (lsv,lls2,g) | (lsv,lls2,cap',n',s',g,phi') <- sgcp t,
                                 cap' == real cap, n' == integer n, s' == s, phi' == phi ] (lsv,lls2,g)
    let (lsv,lls2,g) = last samples

    toFile def ("sgcp-figs/"++ show iter ++".png") $ do
      plot $ line "rate" [sort $ zip (list s) (list g)]
      plot . points "data" $ zip (list s) [if y then 2.5 else (-2.5) | y <- toList phi]

    return (iter+1,lsv,lls2,cap,n,s,g,phi)
