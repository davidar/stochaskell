{-# LANGUAGE RebindableSyntax, MonadComprehensions,
             NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where
import Language.Stochaskell

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )

xyData :: [(Double,Double)]
xyData = [
  (-3.89, -39.69),
  (-3.88, -48.17),
  (-3.37, -34.61),
  (-3.16, -33.06),
  (-2.69, -21.92),
  (-2.68, -21.81),
  (-2.53, -23.35),
  (-2.52, -17.84),
  (-2.48, -22.47),
  (-2.35, -17.95),
  (-2.24, -12.22),
  (-2.23, -21.58),
  (-1.85,  -6.26),
  (-1.69, -11.65),
  (-1.61,  -3.83),
  (-1.57,  -2.18),
  (-1.45, -12.09),
  (-1.29,  -8.01),
  (-1.00,  -7.67),
  (-0.97,  -7.01),
  (-0.91,  -6.13),
  (-0.44,  -3.03),
  (-0.32,   0.72),
  (-0.13,   1.71),
  (-0.01,  -2.58),
  ( 0.03,   2.05),
  ( 0.04,   2.38),
  ( 0.07,   1.28),
  ( 0.08,  -0.37),
  ( 0.15,   6.35),
  ( 0.31,  -0.15),
  ( 0.80,   6.37),
  ( 0.84,   2.51),
  ( 1.00,   4.06),
  ( 1.05,   9.79),
  ( 1.17,   0.54),
  ( 1.28,  16.62),
  ( 1.43,   2.09),
  ( 1.52,   4.29),
  ( 1.67,   8.92),
  ( 1.80,   6.28),
  ( 2.37,  11.48),
  ( 2.40,  10.48),
  ( 3.45,  15.14),
  ( 3.45,  13.49),
  ( 3.61,  13.71),
  ( 3.61,  14.59),
  ( 3.89,  12.95),
  ( 3.91,   8.54),
  ( 3.96,  14.60)]

designMatrix :: Z -> Z -> RVec -> RMat
designMatrix n d x = matrix [ let p = cast (j-1) in (x!i)**p
                            | i <- 1...n, j <- 1...(d+1) ]

poly :: Z -> Z -> P (RVec,R,RVec,RVec)
poly n d = do
  x <- joint vector [ uniform (-4) 4 | i <- 1...n ]
  let design = designMatrix n d x

  let v = 10
  alpha <- invGamma 1 v

  let mu = vector [ 0 | i <- 1...(d+1) ]
      cov = alpha *> inv (tr' design <> design)
  beta <- normalChol (d+1) mu cov

  let z = design #> beta
  y <- joint vector [ normal (z!i) (sqrt v) | i <- 1...n ]

  return (x,alpha,beta,y)

model :: Z -> P (RVec,Z,R,RVec,RVec)
model n = do
  d <- geometric 1 (1/10)
  (x,alpha,beta,y) <- poly n d
  return (x,d,alpha,beta,y)

jump :: (RVec,Z,R,RVec,RVec) -> P (RVec,Z,R,RVec,RVec)
jump (x,d,alpha,beta,y) = do
  d' <- mixture [(1/2, return (d + 1))
                ,(1/2, return (if d > 1 then d - 1 else d))]
  let beta0 =    vector [ if i <= (d+1) then beta!i else 0 | i <- 1...(d'+1) ] :: RVec
  beta' <- joint vector [ normal (beta0!i) (sqrt 10)       | i <- 1...(d'+1) ]
  return (x,d',alpha,beta',y)

main :: IO ()
main = do
  let n =  integer (length  xyData)
      xData = list (map fst xyData)
      yData = list (map snd xyData)

  let d0 = 1
  samples0 <- hmcStan 1000 [ (alpha,beta) | (x,alpha,beta,y) <- poly n d0,
                                            x == xData, y == yData ]
  plotPoly 0 n xData yData d0 samples0
  let (alpha0,beta0) = last samples0

  loop (1,d0,alpha0,beta0) $ \(t,d,alpha,beta) -> do
    print (t,d,alpha,beta)
    (_,d',alphaMH,betaMH,_) <- chain 1000 (model n `mh` jump) (xData,d,alpha,beta,yData)
    samples <- hmcStanInit 1000 [ (alpha,beta) | (x,alpha,beta,y) <- poly n d',
                                                 x == xData, y == yData ]
                           (alphaMH,betaMH)
    plotPoly t n xData yData d' samples
    let (alpha',beta') = last samples
    return (t+1,d',alpha',beta')
  return ()

plotPoly :: Int -> Z -> RVec -> RVec -> Z -> [(R,RVec)] -> IO ()
plotPoly t n xData yData d' samples =
  toFile def ("poly-figs/"++ show t ++".png") $ do
    plot $ points "data" (list xData `zip` list yData)
    setColors [black `withOpacity` 0.02]
    plot $ line ("posterior d="++ show d') $
      map (sort . zip (list xData) . list . extract) samples
  where design = designMatrix n d' xData
        extract (_,beta) = design #> beta
