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

stickBreak :: R -> P RVec
stickBreak alpha = do
  sticks <- joint vector [ beta 1 alpha | _ <- 1...infinity ]
  let rems = scanl f 1 sticks where f r a = r * (1 - a)
      probs = vector [ (sticks!i) * (rems!i) | i <- 1...infinity ]
  return probs

dirichletProcess :: R -> P R -> P (P R)
dirichletProcess alpha base = do
  probs <- stickBreak alpha
  atoms <- joint vector [ base | _ <- 1...infinity ] :: P RVec
  let randomProcess = do
        stick <- pmf probs
        return (atoms!stick)
  return randomProcess

model :: Z -> P (RVec,RVec)
model n = do
  paramDist <- dirichletProcess 1 (uniform 0 100)
  params <- joint vector [ paramDist             | obs <- 1...n ]
  values <- joint vector [ normal (params!obs) 1 | obs <- 1...n ]
  return (params,values)

main :: IO ()
main = putStrLn $ churchProgram (model 1000)
