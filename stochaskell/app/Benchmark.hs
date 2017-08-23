{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction,
             RebindableSyntax, TypeFamilies #-}

module Main where
import Language.Stochaskell

import Data.Time.Clock.POSIX
import Graphics.Rendering.Chart.Easy hiding ((...),(<.>))
import Graphics.Rendering.Chart.Backend.Cairo

model :: Z -> Z -> P (RMat,RVec,R,BVec)
model n d = do
  -- TODO: automatic vectorisation
  --x <- joint matrix [ uniform (-5) 5 | i <- 1...n, j <- 1...d ]
  x <- uniforms (matrix [ -5 | i <- 1...n, j <- 1...d ])
                (matrix [  5 | i <- 1...n, j <- 1...d ])
  --w <- joint vector [ normal 0 3 | j <- 1...d ]
  w <- normals (vector [ 0 | j <- 1...d ]) (vector [ 3 | j <- 1...d ])
  b <- normal 0 3
  -- TODO: automatic broadcasting
  let z = (x #> w) + vector [ b | i <- 1...n ]
  y <- bernoulliLogits z
  return (x,w,b,y)

mean xs = sum xs / fromIntegral (length xs)

main = do
  let n = 10000; d = 2
  (xData,wTrue,bTrue,yData) <- simulate (model n d)

  toFile def "logreg-data.png" $ do
    plot $ points "true"  $ map (\(a:b:[]) -> (a,b)) (list xData `selectItems` list yData)
    plot $ points "false" $ map (\(a:b:[]) -> (a,b)) (list xData `selectItems` (not <$> list yData))

  let post = [ (w,b) | (x,w,b,y) <- model n d, x == xData, y == yData ]
      numSteps = 10
      stepSize = 0.01

  ticStan <- getPOSIXTime
  let method = defaultStanMethod
        { numSamples = 1000
        , numWarmup = 1000
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

  ticPyMC3 <- getPOSIXTime
  samples <- hmcPyMC3 1000 post
  tocPyMC3 <- getPOSIXTime
  let wPyMC3 = mean (map fst samples)
      bPyMC3 = mean (map snd samples)

  ticEdward <- getPOSIXTime
  samples <- drop 1000 <$> hmcEdward 2000 numSteps stepSize post
  tocEdward <- getPOSIXTime
  let wEdward = mean (map fst samples)
      bEdward = mean (map snd samples)

  putStrLn "==="
  putStrLn $ "TRUTH:\t"++  show (wTrue,   bTrue)
  putStrLn $ "STAN:\t"++   show (wStan,   bStan)   ++" took "++ show (tocStan   - ticStan)
  putStrLn $ "PYMC3:\t"++  show (wPyMC3,  bPyMC3)  ++" took "++ show (tocPyMC3  - ticPyMC3)
  putStrLn $ "EDWARD:\t"++ show (wEdward, bEdward) ++" took "++ show (tocEdward - ticEdward)
