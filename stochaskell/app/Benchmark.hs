{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction,
             RebindableSyntax, TypeFamilies #-}

module Main where
import Language.Stochaskell

import qualified Data.Expression as E
import Data.Expression.Eval
import Data.Maybe
import Data.Time.Clock.POSIX

logreg :: Z -> Z -> P (RMat,RVec,R,BVec)
logreg n d = do
  -- TODO: automatic vectorisation
  --x <- joint matrix [ uniform (-5) 5 | i <- 1...n, j <- 1...d ]
  x <- uniforms (matrix [ -10000 | i <- 1...n, j <- 1...d ])
                (matrix [  10000 | i <- 1...n, j <- 1...d ])
  --w <- joint vector [ normal 0 3 | j <- 1...d ]
  w <- normals (vector [ 0 | j <- 1...d ]) (vector [ 3 | j <- 1...d ])
  b <- normal 0 3
  let z = bsxfun (+) (x #> w) b
  y <- bernoulliLogits z
  return (x,w,b,y)

poly :: Z -> Z -> P (RVec,R,RVec,RVec)
poly n d = do
  --x <- joint vector [ uniform (-4) 4 | i <- 1...n ]
  x <- uniforms (vector [ -4 | i <- 1...n ])
                (vector [  4 | i <- 1...n ])
  let design = matrix [ let p = cast (j-1) in (x!i)**p
                      | i <- 1...n, j <- 1...(d+1) ]
  (a,b,y) <- nlm' n (d+1) design
  return (x,a,b,y)

nlm :: Z -> Z -> P (RMat,R,RVec,RVec)
nlm n k = do
  x <- uniforms (matrix [ -10000 | i <- 1...n, j <- 1...k ])
                (matrix [  10000 | i <- 1...n, j <- 1...k ])
  (a,b,y) <- nlm' n k x
  return (x,a,b,y)

nlm' :: Z -> Z -> RMat -> P (R,RVec,RVec)
nlm' n k x = do
  let v = 10
  alpha <- invGamma 1 1

  let mu = vector [ 0 | i <- 1...k ]
      cov = (v*alpha) *> inv (tr' x <> x)
  beta <- normalChol k mu cov

  let z = x #> beta
  --y <- joint vector [ normal (z!i) (sqrt v) | i <- 1...n ]
  y <- normals z (vector [ sqrt v | i <- 1...n ])

  return (alpha,beta,y)

distEucSq :: RVec -> RVec -> RMat
distEucSq a b = bsxfun (+) (asColumn $ square a) (asRow $ square b) - (2 *> outer a b)

kernelSE' :: R -> R -> Z -> RVec -> RVec -> RMat
kernelSE' lsv lls2 n a b =
  --matrix [ exp (lsv - ((a!i) - (b!j))^2 / (2 * exp lls2))
  --         + if (a!i) == (b!j) then 0.01 else 0
  --       | i <- 1...n, j <- 1...n ]
  exp (bsxfun (-) lsv (bsxfun (/) (distEucSq a b) (2 * exp lls2)))
  + diag (vector [ 0.01 | i <- 1...n ])

gpClassifier :: (Z -> RVec -> RVec -> RMat) -> Z -> RVec -> P (RVec,BVec)
gpClassifier kernel n s = do
  let mu  = vector [ 0 | _ <- 1...n ]
      cov = kernel n s s
  g <- normalChol n mu cov
  phi <- bernoulliLogits g
  return (g,phi)

gpclas :: Z -> P (R,R,RVec,RVec,BVec)
gpclas n = do
  lsv <- normal 0 1
  lls2 <- normal (log 100) 2
  x <- uniforms (vector [  0 | i <- 1...n ])
                (vector [ 50 | i <- 1...n ])
  (g,phi) <- gpClassifier (kernelSE' lsv lls2) n x
  return (lsv,lls2,x,g,phi)

covtype :: IO ()
covtype = do
  let n = 581012; d = 54
  --(xData,wTrue,bTrue,yData) <- simulate (model n d)
  table <- readRealMatrix "data/covtype.std.data" (1,n) (1,55)
  let xData = E.const $ table `slice` [[i,j] | i <- 1...n, j <- 1...d]
      yData = E.const . binarize (1 ==) $ table `slice` [[i,55] | i <- 1...n]

  let post = [ (w,b) | (x,w,b,y) <- logreg n d, x == xData, y == yData ]
      stepSize = 0.0001

  msgStan   <- benchStanHMC   100 10 stepSize post Nothing
  msgPyMC3  <- benchPyMC3HMC  100 10 stepSize post Nothing
  msgEdward <- benchEdwardHMC 100 10 stepSize post Nothing

  putStrLn "==="
  --putStrLn $ "TRUTH:\t"++  show (wTrue,   bTrue)
  putStrLn msgStan
  putStrLn msgPyMC3
  putStrLn msgEdward

poly' :: IO ()
poly' = do
  let n = 1000; d = 7
  (xData,alphaTrue,betaTrue,yData) <- simulate (poly n d)
  let design = matrix [ let p = cast (j-1) in (xData!i)**p
                      | i <- 1...n, j <- 1...(d+1) ] :: RMat
      design' = E.const . fromJust $ eval_ design
  let post = [ (a,b) | (x,a,b,y) <- nlm n (d+1), x == design', y == yData ]
      stepSize = 0.1
  (_,alphaInit,betaInit,_) <- simulate (poly n d)
  let init = Just (alphaInit, betaInit)
  msgEdward <- benchEdwardHMC 1000 10 stepSize post init
  msgPyMC3  <- benchPyMC3HMC  1000 10 stepSize post init
  msgStan   <- benchStanHMC   1000 10 stepSize post init
  putStrLn "==="
  putStrLn $ "TRUTH:\t"++ show (alphaTrue, betaTrue)
  putStrLn msgStan
  putStrLn msgPyMC3
  putStrLn msgEdward

gpc :: IO ()
gpc = do
  let n = 100
  (lsvTrue,lls2True,xData,_,zData) <- simulate (gpclas n)
  let post = [ (v,l) | (v,l,x,_,z) <- gpclas n, x == xData, z == zData ]
      stepSize = 0.1
  msgEdward <- benchEdwardHMC 1000 10 stepSize post Nothing
  msgPyMC3  <- benchPyMC3HMC  1000 10 stepSize post Nothing
  msgStan   <- benchStanHMC   1000 10 stepSize post Nothing
  putStrLn "==="
  putStrLn $ "TRUTH:\t"++ show (lsvTrue, lls2True)
  putStrLn msgStan
  putStrLn msgPyMC3
  putStrLn msgEdward

benchStanHMC numSamp numSteps stepSize p init = do
  ticStan <- getPOSIXTime
  let method = defaultStanMethod
        { numSamples = numSamp
        , numWarmup = 0
        , adaptEngaged = False
        , hmcEngine = StanStaticHMCEngine
          { intTime = numSteps * stepSize }
        , hmcMetric = StanUnitEMetric
        , hmcStepSize = stepSize
        }
  samples <- runStan method p init
  tocStan <- getPOSIXTime
  let aStan = mean (map fst samples)
      bStan = mean (map snd samples)
  return $ "STAN:\t"++   show (aStan,   bStan)   ++" took "++ show (tocStan   - ticStan)

benchPyMC3HMC numSamp numSteps stepSize p init = do
  putStrLn $ pmProgram p
  ticPyMC3 <- getPOSIXTime
  let method = defaultPyMC3Inference
        { pmDraws = numSamp
        , pmStep = Just HamiltonianMC
          { pathLength = numSteps * stepSize
          , stepRand = "lambda _:"++ show stepSize
          , stepScale = stepSize
          }
        , pmInit = Nothing
        , pmTune = 0
        }
  samples <- runPyMC3 method p init
  tocPyMC3 <- getPOSIXTime
  let aPyMC3 = mean (map fst samples)
      bPyMC3 = mean (map snd samples)
  return $ "PYMC3:\t"++  show (aPyMC3,  bPyMC3)  ++" took "++ show (tocPyMC3  - ticPyMC3)

benchEdwardHMC numSamp numSteps stepSize p init = do
  putStrLn $ edProgram numSamp numSteps stepSize p init
  ticEdward <- getPOSIXTime
  samples <- hmcEdward numSamp numSteps stepSize p init
  tocEdward <- getPOSIXTime
  let aEdward = mean (map fst samples)
      bEdward = mean (map snd samples)
  return $ "EDWARD:\t"++ show (aEdward, bEdward) ++" took "++ show (tocEdward - ticEdward)

main = gpc
