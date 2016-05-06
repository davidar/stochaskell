{-# LANGUAGE RebindableSyntax, MonadComprehensions,
             NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Prelude hiding ((==),(/=),(<),(>),(<=),(>=),foldr)

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Control.Monad.Guard
import Data.Array.Abstract
import Data.Bool.Num ()
import Data.Boolean.Overload
import Data.Expression
import Data.Expression.Const
import Data.Expression.Eval
import Data.List hiding (foldr)
import Data.Program
import Data.Random.Distribution.Abstract
import Language.Stan

polyroots :: RVec -> R -> R
polyroots as x = foldr f 1 as
  where f a r = (x - a) * r

poly :: Z -> Z -> P (RVec,RVec,RVec,RVec)
poly d n = do
  r <- joint vector [ uniform (-1) 1         | i <- 1...d ]
  x <- joint vector [ uniform (-1) 1         | i <- 1...n ]
  let z =    vector [ 10 * polyroots r (x!i) | i <- 1...n ]
  y <- joint vector [ normal (z!i) 1         | i <- 1...n ]
  return (r,x,z,y)

model :: Z -> P (Z,RVec,RVec,RVec,RVec)
model n = do
  d <- geometric 1 (1/10)
  (r,x,z,y) <- poly d n
  return (d,r,x,z,y)

jump :: Z -> (Z,RVec,RVec,RVec,RVec) -> P (Z,RVec,RVec,RVec,RVec)
jump n (_,_,x,_,y) = do
  d' <- geometric 1 (1/10)
  r' <- joint vector [ uniform (-1) 1          | i <- 1...d' ]
  let z' =    vector [ 10 * polyroots r' (x!i) | i <- 1...n  ]
  return (d',r',x,z',y)

main :: IO ()
main = do
  let n = 100; dTrue = 5
  (_,xData,zData,yData) <- sampleP (poly dTrue n)
  let d0 = 1
  (r0,z0) <- last <$> hmcStan [ (r,z) | (r,x,z,y) <- poly d0 n, x == xData, y == yData ]
  loop (1,d0,r0,z0) $ \(t,d,r,z) -> do
    (d',rMH,_,zMH,_) <- chain 1000 (model n `mh` jump n) (d,r,xData,z,yData)
    samples <- [ (r,z) | (r,x,z,y) <- poly d' n, x == xData, y == yData ] `hmcStanInit` (rMH,zMH)
    plotPoly dTrue t xData zData yData d' zMH samples
    let (r',z') = last samples
    return (t+1,d',r',z')

plotPoly :: Integer -> Integer -> RVec -> RVec -> RVec
         -> Z -> RVec -> [(RVec,RVec)] -> IO ()
plotPoly dTrue t xData zData yData d' zMH samples =
  toFile def ("poly_t"++ show t ++".png") $ do
    plot $ points "data" (list xData `zip` list yData)
    plot $ line ("truth d="++ show dTrue) [sort $ list xData `zip` list zData ]
    plot $ line ("sample d="++ show (integer $ eval_ d'))
      [ sort $ zip (list xData) (list zMH) ]
    setColors [black `withOpacity` 0.05]
    plot $ line ("posterior d="++ show (integer $ eval_ d')) $
      map (sort . zip (list xData) . list . snd) samples
