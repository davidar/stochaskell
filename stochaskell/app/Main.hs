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

horner :: RVec -> R -> R
horner ws x = foldr f 0 ws
  where f w r = w + x * r -- (w:ws) -> w + x * rec ws

poly :: Z -> Z -> P (RVec,RVec,RVec,RVec)
poly d n = do
  w <- joint vector [ normal 0 1 | i <- 1...(d + 1) ]
  x <- joint vector [ uniform (-1.5) 1.5 | i <- 1...n ]
  let z =    vector [ horner w (x!i)     | i <- 1...n ]
  y <- joint vector [ normal (z!i) 10    | i <- 1...n ]
  return (x,w,z,y)

model :: Z -> P (Z,RVec,RVec,RVec,RVec)
model n = do
  d <- geometric (1/2)
  (x,w,z,y) <- poly d n
  return (d,x,w,z,y)

jump :: Z -> (Z,RVec,RVec,RVec,RVec) -> P (Z,RVec,RVec,RVec,RVec)
jump n (d,x,w,_,y) = do
  d' <- categorical [(1/2, d + 1)
                    ,(1/2, if d > 1 then d - 1 else d)]
  m <- normal 0 1
  let w' = vector [ if i > d then m else w!i | i <- 1...d' ]
      z' = vector [ horner w' (x!i) | i <- 1...n ]
  return (d',x,w',z',y)

main :: IO ()
main = do
  let n = 100
  (xData,_,zData,yData) <- sampleP (poly 7 n)
  loop (0,1) $ \(t,d) -> do
    samples <- hmcStan [ (w,z) | (x,w,z,y) <- poly d n, x == xData, y == yData ]
    let (w,z) = last samples
    (d',_,_,z',_) <- chain 100 (model n `mh` jump n) (d,xData,w,z,yData)
    plotPoly 7 t xData zData yData d samples d' z'
    return (t+1,d')

plotPoly :: Integer -> Integer -> RVec -> RVec -> RVec
         -> Z -> [(RVec,RVec)] -> Z -> RVec -> IO ()
plotPoly d0 t xData zData yData d samples d' z' =
  toFile def ("poly_t"++ show t ++".png") $ do
    plot $ points "data" (list xData `zip` list yData)
    plot $ line ("truth d="++ show d0) [sort $ list xData `zip` list zData ]
    plot $ line ("sample d="++ show (integer $ eval_ d'))
      [ sort $ zip (list xData) (list z') ]
    setColors [black `withOpacity` 0.05]
    plot $ line ("posterior d="++ show (integer $ eval_ d)) $
      map (sort . zip (list xData) . list . snd) samples
