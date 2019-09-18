{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction,
             RebindableSyntax, TypeFamilies #-}

module Main where
import Language.Stochaskell
import Language.Stochaskell.Expression
import Language.Edward
import Language.PyMC3
import Language.Stan

logreg :: Z -> Z -> P (RMat,RVec,R,BVec)
logreg n d = do
  -- TODO: automatic vectorisation
  --x <- joint matrix [ uniform (-5) 5 | i <- 1...n, j <- 1...d ]
  x <- uniforms (matrix [ -10000 | i <- 1...n, j <- 1...d ])
                (matrix [  10000 | i <- 1...n, j <- 1...d ])
  --w <- joint vector [ normal 0 3 | j <- 1...d ]
  w <- normals (vector [ 0 | j <- 1...d ]) (vector [ 3 | j <- 1...d ])
  b <- normal 0 3
  let z = (x #> w) + cast b
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
distEucSq a b = asColumn (square a) + asRow (square b) - (2 *> outer a b)

kernelSE' :: R -> R -> Z -> RVec -> RVec -> RMat
kernelSE' lsv lls2 n a b =
  --matrix [ exp (lsv - ((a!i) - (b!j))^2 / (2 * exp lls2))
  --         + if (a!i) == (b!j) then 0.01 else 0
  --       | i <- 1...n, j <- 1...n ]
  exp (cast lsv - distEucSq a b / cast (2 * exp lls2))
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

measerr :: Z -> R -> P (R,R,RVec,RVec,R,R,R,RVec)
measerr n tau = do
  xMu <- uniform (-10) 10
  xSigma <- uniform 0 10
  x <- normals (vector [ xMu | i <- 1...n ])
               (vector [ xSigma | i <- 1...n ])
  xMeas <- normals x (vector [ tau | i <- 1...n ])
  alpha <- normal 0 10
  beta <- normal 0 10
  sigma <- truncated 0 infinity (cauchy 0 5)
  y <- normals (cast alpha + (beta *> x)) (vector [ sigma | i <- 1...n ])
  return (xMu,xSigma,x,xMeas,alpha,beta,sigma,y)

covPrior :: Z -> P RMat
covPrior n = do
  tau <- joint vector [ truncated 0 infinity (cauchy 0 2.5) | i <- 1...n ]
  corr <- corrLKJ 2 (1,n)
  -- TODO: automatically recognise common forms
  --let betaSigma = diag tau <> corr <> diag tau
  return (qfDiag corr tau)

birats :: Z -> Z -> P (RVec,RVec,RMat,RMat,R,RMat)
birats n t = do
  x <- uniforms (vector [  0 | j <- 1...t ])
                (vector [ 50 | j <- 1...t ])
  betaMu <- normals (vector [   0 | _ <- 1...2 ])
                    (vector [ 100 | _ <- 1...2 ])
  betaSigma <- covPrior 2
  beta <- normalsChol n 2 betaMu betaSigma
  let beta' = tr' beta
      beta1 = beta'!1
      beta2 = beta'!2
  --let yMu = matrix [ (beta1!i) + (beta2!i) * (x!j) | i <- 1...n, j <- 1...t ]
  let yMu = asColumn beta1 + outer beta2 x
  ySigma <- truncated 0 infinity (cauchy 0 2.5) -- TODO: check this still works
  y <- normals yMu (matrix [ ySigma | i <- 1...n, j <- 1...t ])
  return (x,betaMu,betaSigma,beta,ySigma,y)

covtype :: IO ()
covtype = do
  let n = 581012; d = 54
  --(xData,wTrue,bTrue,yData) <- simulate (model n d)
  table <- readRealMatrix "data/covtype.std.data"
  let xData = constExpr $ table `slice` [[i,j] | i <- 1...n, j <- 1...d]
      yData = constExpr . binarize (1 ==) $ table `slice` [[i,55] | i <- 1...n]

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
      design' = constExpr . fromRight $ eval_ design
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

measerr' :: IO ()
measerr' = do
  let n = 1000; tau = 0.1
  (xMuTrue,xSigmaTrue,xTrue,xMeasData,alphaTrue,betaTrue,sigmaTrue,yData) <- simulate (measerr n tau)
  let post = [ (xm,xs,x,a,b,s) | (xm,xs,x,xx,a,b,s,y) <- measerr n tau, xx == xMeasData, y == yData ]
  (xMuInit,xSigmaInit,xInit,_,alphaInit,betaInit,sigmaInit,_) <- simulate (measerr n tau)
  let init = (xMuInit,xSigmaInit,xInit,alphaInit,betaInit,sigmaInit)
  samplesStan <- hmcStanInit 1000 post init
  putStrLn $ pmProgram post
  samplesPyMC3 <- runPyMC3 defaultPyMC3Inference{pmDraws=1000,pmInit=Nothing,pmTune=1000} post (Just init)
  putStrLn "==="
  putStrLn $ "TRUTH:\t"++ show (xMuTrue,xSigmaTrue,xTrue,alphaTrue,betaTrue,sigmaTrue)
  print $ last samplesStan
  print $ last samplesPyMC3

birats' :: IO ()
birats' = do
  let n = 100; t = 10
  (xData,bmTrue,bsTrue,bTrue,yvTrue,yData) <- simulate (birats n t)
  let post = [ (bm,bs,b,yv) | (x,bm,bs,b,yv,y) <- birats n t, x == xData, y == yData ]
  (_,bmInit,bsInit,bInit,yvInit,_) <- simulate (birats n t)
  let init = (bmInit,bsInit,bInit,yvInit)
  samplesStan <- hmcStanInit 1000 post init
  putStrLn $ pmProgram post
  samplesPyMC3 <- runPyMC3 defaultPyMC3Inference{pmDraws=1000,pmInit=Nothing,pmTune=1000} post (Just init)
  putStrLn "==="
  putStrLn $ "TRUTH:\t"++ show (bmTrue,yvTrue)
  let (bmSample,_,_,yvSample) = last samplesStan
  print (bmSample,yvSample)
  let (bmSample,_,_,yvSample) = last samplesPyMC3
  print (bmSample,yvSample)

benchStanHMC numSamp numSteps stepSize p init = do
  t <- tic
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
  s <- toc t
  let means = map mean . transpose $ map (map (fromRight . eval_ . Expression) . fromExprTuple) samples
  return $ "STAN:\t"++ show means ++" took "++ show s

benchPyMC3HMC numSamp numSteps stepSize p init = do
  putStrLn $ pmProgram p
  t <- tic
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
  s <- toc t
  let aPyMC3 = mean (map fst samples)
      bPyMC3 = mean (map snd samples)
  return $ "PYMC3:\t"++  show (aPyMC3,  bPyMC3)  ++" took "++ show s

benchEdwardHMC numSamp numSteps stepSize p init = do
  putStrLn $ edProgram numSamp numSteps stepSize p init
  t <- tic
  samples <- hmcEdward numSamp numSteps stepSize p init
  s <- toc t
  let aEdward = mean (map fst samples)
      bEdward = mean (map snd samples)
  return $ "EDWARD:\t"++ show (aEdward, bEdward) ++" took "++ show s

main = birats'
