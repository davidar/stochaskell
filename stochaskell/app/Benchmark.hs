{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction,
             RebindableSyntax, TypeFamilies #-}

module Main where
import Language.Stochaskell

import qualified Data.Expression as E
import Data.Time.Clock.POSIX

model :: Z -> Z -> P (RMat,RVec,R,BVec)
model n d = do
  -- TODO: automatic vectorisation
  --x <- joint matrix [ uniform (-5) 5 | i <- 1...n, j <- 1...d ]
  x <- uniforms (matrix [ -10000 | i <- 1...n, j <- 1...d ])
                (matrix [  10000 | i <- 1...n, j <- 1...d ])
  --w <- joint vector [ normal 0 3 | j <- 1...d ]
  w <- normals (vector [ 0 | j <- 1...d ]) (vector [ 3 | j <- 1...d ])
  b <- normal 0 3
  -- TODO: automatic broadcasting
  let z = (x #> w) + vector [ b | i <- 1...n ]
  y <- bernoulliLogits z
  return (x,w,b,y)

main = do
  let n = 581012; d = 54
  --(xData,wTrue,bTrue,yData) <- simulate (model n d)
  table <- readRealMatrix "data/covtype.std.data" (1,n) (1,55)
  let xData = E.const $ table `slice` [[i,j] | i <- 1...n, j <- 1...d]
      yData = E.const . binarize (1 ==) $ table `slice` [[i,55] | i <- 1...n]

  let post = [ (w,b) | (x,w,b,y) <- model n d, x == xData, y == yData ]
      numSteps = 10
      stepSize = 0.0001

  ticStan <- getPOSIXTime
  let method = defaultStanMethod
        { numSamples = 100
        , numWarmup = 0
        , adaptEngaged = False
        , hmcEngine = StanStaticHMCEngine
          { intTime = numSteps * stepSize }
        , hmcMetric = StanUnitEMetric
        , hmcStepSize = stepSize
        }
  samples <- runStan method post Nothing
  tocStan <- getPOSIXTime
  let wStan = mean (map fst samples)
      bStan = mean (map snd samples)

  putStrLn $ pmProgram post
  ticPyMC3 <- getPOSIXTime
  let method = defaultPyMC3Inference
        { pmDraws = 100
        , pmStep = Just HamiltonianMC
          { pathLength = numSteps * stepSize
          , stepRand = "lambda _:"++ show stepSize
          , stepScale = stepSize
          }
        , pmInit = Nothing
        , pmTune = 0
        }
  samples <- runPyMC3 method post
  tocPyMC3 <- getPOSIXTime
  let wPyMC3 = mean (map fst samples)
      bPyMC3 = mean (map snd samples)

  putStrLn $ edProgram 100 numSteps stepSize post
  ticEdward <- getPOSIXTime
  samples <- hmcEdward 100 numSteps stepSize post
  tocEdward <- getPOSIXTime
  let wEdward = mean (map fst samples)
      bEdward = mean (map snd samples)

  putStrLn "==="
  --putStrLn $ "TRUTH:\t"++  show (wTrue,   bTrue)
  putStrLn $ "STAN:\t"++   show (wStan,   bStan)   ++" took "++ show (tocStan   - ticStan)
  putStrLn $ "PYMC3:\t"++  show (wPyMC3,  bPyMC3)  ++" took "++ show (tocPyMC3  - ticPyMC3)
  putStrLn $ "EDWARD:\t"++ show (wEdward, bEdward) ++" took "++ show (tocEdward - ticEdward)
