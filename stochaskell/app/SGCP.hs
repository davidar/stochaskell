{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where

import Data.List (sort)
import Graphics.Rendering.Chart.Easy (plot,line,points,def)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Language.Stochaskell

kernelSE lsv lls2 a b =
  exp (lsv - (a - b)^2 / (2 * exp lls2))
  + if a == b then 1e-6 else 0

gpClassifier :: (R -> R -> R) -> Z -> RVec -> P (RVec,BVec)
gpClassifier kernel n s = do
  let mu  = vector [ 0 | _ <- 1...n ]
      cov = matrix [ kernel (s!i) (s!j) | i <- 1...n, j <- 1...n ]
  g <- normalChol n mu cov
  phi <- joint vector [ bernoulliLogit (g!i) | i <- 1...n ]
  return (g,phi)

poissonProcess' :: R -> R -> P (Z,RVec)
poissonProcess' rate t = do
  n <- poisson (rate * t)
  s <- orderedSample n (uniform 0 t)
  return (n,s)

type State = (R,R,R,Z,RVec,RVec,BVec)

dim :: State -> Z
dim (_,_,_,n,_,_,_) = n

sgcp :: R -> P State
sgcp t = do
  lsv <- normal 0 1
  lls2 <- normal (log 100) 2
  cap <- gamma 1 1
  (n,s) <- poissonProcess' cap t
  let kernel = kernelSE lsv lls2
  (g,phi) <- gpClassifier kernel n s
  return (lsv, lls2, cap, n, s, g, phi)

stepDown :: Z -> Z -> State -> P State
stepDown k n' (lsv,lls2,cap,n,s,g,phi) = do
  i <- uniform (k+1) n'
  let s'   = deleteIndex s i
      g'   = deleteIndex g i
      phi' = deleteIndex phi i
  return (lsv,lls2,cap,n',s',g',phi')

stepUp :: R -> Z -> Z -> State -> P State
stepUp t k n' (lsv,lls2,cap,n,s,g,phi) = do
  x <- uniform 0 t
  let kernel = kernelSE lsv lls2
  z <- normalCond n kernel s g x
  let i = findSortedInsertIndex x s
      s'   = insertIndex s i x
      g'   = insertIndex g i z
      phi' = insertIndex phi i false
  return (lsv,lls2,cap,n',s',g',phi')

stepN :: R -> Z -> State -> P State
stepN t k state@(lsv,lls2,cap,n,s,g,phi) = mixture'
  [(1/2, stepUp t k (n + 1) state)
  ,(1/2, if n == k then return state
                   else stepDown k (n - 1) state)
  ]

stepS :: Z -> State -> P State
stepS idx (lsv, lls2, cap, n, s, g, phi) = do
  x <- normal (s!idx) (exp (lls2/2))
  let kernel = kernelSE lsv lls2
  z <- normalCond n kernel s g x
  let s' = s `replaceAt` (idx, x)
      g' = g `replaceAt` (idx, z)
  return (lsv, lls2, cap, n, s', g', phi)

stepCap :: R -> State -> P State
stepCap t (lsv, lls2, cap, n, s, g, phi) = do
  let a = cast (1 + n) :: R
  cap' <- gamma a (1 + t)
  return (lsv, lls2, cap', n, s, g, phi)

stepMH :: R -> Z -> State -> P State
stepMH t k state = do
  state <- chain' 10 (sgcp t `mh'` stepN t k) state
  state <- chainRange' (k + 1, dim state) (\i -> sgcp t `mh'` stepS i) state
  state <- (sgcp t `mh'` stepCap t) state
  return state

stepGP :: R -> State -> IO State
stepGP t (lsv,lls2,cap,n,s,g,phi) = do
  samples <- hmcStanInit 10 [ (lsv',lls2',g') | (lsv',lls2',cap',n',s',g',phi') <- sgcp t,
                              cap' == cap, n' == n, s' == s, phi' == phi ] (lsv,lls2,g)
  let (lsv',lls2',g') = last samples
  return (lsv',lls2',cap,n,s,g',phi)

step :: R -> Z -> State -> IO State
step t k state = do
  state <- stepMH t k `runCC` state
  state <- stepGP t state
  return state

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

initialise :: R -> [Double] -> IO State
initialise t dat = do
  let k = integer (length dat)
      cap = real (2 * k / t)
      m = 10
      n = k + m
      phi = fromList $ replicate k True ++ replicate m False :: BVec
  rej <- sequence [ uniform 0 (real t) | _ <- [1..m] ]
  let s = fromList $ dat ++ sort rej :: RVec
  samples <- hmcStan 1000 [ (lsv,lls2,g) | (lsv,lls2,cap',n',s',g,phi') <- sgcp t,
                            cap' == real cap, n' == integer n, s' == s, phi' == phi ]
  let (lsv,lls2,g) = last samples
  return (lsv,lls2,cap,n,s,g,phi)

main :: IO ()
main = do
  let t = 50
  dat <- genData' t
  let k = integer (length dat)
  state <- initialise t dat
  flip (chain 300) (0,state) $ \(iter,state) -> do
    putStrLn $ "*** CURRENT STATE: "++ show state
    (lsv,lls2,cap,n,s,g,phi) <- step t k state
    toFile def ("sgcp-figs/"++ show iter ++".png") $ do
      plot $ line "rate" [sort $ zip (list s) (list g)]
      plot . points "data" $ zip (list s) [if y then 2.5 else (-2.5) | y <- toList phi]
    return (iter+1,(lsv,lls2,cap,n,s,g,phi))
  return ()
