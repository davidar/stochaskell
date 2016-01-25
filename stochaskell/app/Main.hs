{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main where

import Graphics.Rendering.Chart.Easy
    ( plot, line, points, def, setColors, black, withOpacity )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Control.Applicative ()
import Control.Monad.State ( runState )
import Data.Array.Abstract
    ( SquareMatrix(chol), LinearOperator((#>)), Indexable((!)), (...) )
import qualified Data.Bimap as Bimap ()
import Data.Boolean ( IfB(ifB), EqB((==*)) )
import Data.Expression
import Data.List ( isPrefixOf, intercalate, findIndices )
import Data.List.Split ( splitOn )
import Data.Program
import Data.Ratio ( (%) )
import System.Process ( system )
import Language.Stan

stanPBlock0 rName rId (PBlock block revrefs given) =
    "data {\n"++ dat ++"\n}\n\n"++
    "parameters {\n"++ params ++"\n}\n\n"++
    "transformed parameters {\n"++ edefs ++"\n}\n\n"++
    "model {\n"++ pdefs ++"\n}\n"
  where refs = reverse revrefs
        depth = dagLevel $ head block
        ident i n = External (Id (Volatile depth) (show i)) (typePNode n)
        name i n = let idt = ident i n in if idt == rId then rName else stanNodeRef idt
        printRefs f = indent . unlines . filter (/= "") $ zipWith f [0..] refs
        pdefs = printRefs $ \i n -> stanPNode (name i n) n
        params = printRefs $ \i n ->
          if ident i n `elem` map fst given
          then ""
          else stanDecl (name i n) (typePNode n)
        dat = printRefs $ \i n ->
          if ident i n `elem` map fst given
          then stanDecl (name i n) (typePNode n)
          else ""
        edefs = stanDAG (head block)

extractCSV name content = table'
  where rows = lines content
        noComment row = row /= "" && head row /= '#'
        rows' = filter noComment rows
        table = map (splitOn ",") rows'
        prefix = name ++ "."
        cols = findIndices (isPrefixOf prefix) (head table)
        idx = head cols
        len = length cols
        table' = map (take len . drop idx) table

prior n = do
    s <- joint vector $ (\i -> normal 0 1) <$> 1...n
    lsv <- normal 0 1
    lls <- normal (log 10) 1
    w <- joint vector $ (\i -> normal 0 1) <$> 1...n
    let cov = matrix $ m <$> 1...n <*> 1...n
          where m i j = exp (lsv - ((s!i) - (s!j))^2 / (2 * exp (2 * lls))) + ifB (i ==* j) 1e-6 0
        g = chol cov #> w
    phi <- joint vector $ (\i -> bernoulliLogit (g!i)) <$> 1...n
    return (g,phi,s)

inp = map (%10) [0..100]
dat = [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
       0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
       1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1,
       0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
       1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0]

posterior = do
    (g,phi,s) <- prior 101
    assume phi dat
    assume s inp
    return g

runStan p = do
    putStrLn "--- Generating Stan code ---"
    putStrLn model
    writeFile "genmodel.stan" model
    writeFile "genmodel.data" constrs
    system "stan genmodel"
    system "./genmodel sample data file=genmodel.data"
    content <- readFile "output.csv"
    let table = extractCSV collect content
        table' = map (map read) (tail table) :: [[Double]]
    return table'
  where ((r,_), pblock) = runState (fromProgE p) emptyPBlock
        model = stanPBlock0 "RESULT" r pblock
        collect = case r of
          (External _ _) -> "RESULT"
          _ -> stanNodeRef r
        constrs = unlines . map f $ constraints pblock
          where f (n,ar) = stanNodeRef n ++" <- c("++ intercalate ", " (map g ar) ++")"
                g = stanNodeRef . Constant

main = do
  samples <- runStan posterior
  let xs = map fromRational inp :: [Double]
  toFile def "out.png" $ do
    plot . points "data" $ zip xs [5 * (fromRational y - 0.5) :: Double | y <- dat]
    setColors [black `withOpacity` 0.1]
    plot . line "samples" $ map (zip xs) samples
