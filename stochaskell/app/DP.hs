{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Prelude hiding ((==),(/=),(<),(>),(<=),(>=),foldl,foldr,scanl,scanr)

import Control.Applicative ()
import Data.Array.Abstract
import Data.Expression
import Data.Number.Transfinite
import Data.Program
import Data.Random.Distribution.Abstract
import Language.Church

naturalDP :: R -> P RVec
naturalDP alpha = do
  sticks <- joint vector [ beta 1 alpha | _ <- 1...infinity ]
  let rems = scanl f 1 sticks where f r a = r * (1 - a)
      probs = vector [ (sticks!i) * (rems!i) | i <- 1...infinity ]
  return probs

model :: Z -> P (RVec,ZVec,RVec)
model n = do
  probs <- naturalDP 1
  params  <- joint vector [ uniform 0 100 | cls <- 1...infinity ]
  classes <- joint vector [ pmf probs     | obs <- 1...n ]
  values  <- joint vector [ let cls = classes!obs
                            in normal (params!cls) 1
                          | obs <- 1...n ]
  return (params,classes,values)

main :: IO ()
main = putStrLn $ churchProgram (model 1000)
