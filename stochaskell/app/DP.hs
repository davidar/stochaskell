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

main :: IO ()
main = putStrLn $ churchProgram (naturalDP 1)
