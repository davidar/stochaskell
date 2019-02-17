{-|
Description : Church integration
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Church
  ( mhChurch
  , simChurchVec
  ) where

import Data.Array.Abstract
import Data.Expression
import Data.Expression.Const
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Program
import Data.Ratio
import GHC.Exts
import System.Directory
import System.IO.Temp
import System.Process
import Util

spaced :: (a -> String) -> [a] -> String
spaced f = unwords . map f

churchId :: Id -> String
churchId = show

churchConstVal :: ConstVal -> String
churchConstVal val = case dimension val of
  0 -> show val
  1 -> "(list "++ churchConstVal `spaced` toList val ++")"

churchNodeRef :: NodeRef -> String
churchNodeRef (Var s _) = churchId s
churchNodeRef (Const c _) = churchConstVal c
churchNodeRef (Index f js) =
  "("++ churchNodeRef f ++" "++ churchNodeRef `spaced` reverse js ++")"

churchBuiltinFunctions =
  [("ifThenElse", "if")
  ]

churchBuiltinDistributions =
  [("normal", "gaussian")
  ]

churchPrelude :: String
churchPrelude = unlines
  ["(define (pmf probs)"
  ,"  (letrec ((go (lambda (j u)"
  ,"                 (if (<= u (probs j)) j"
  ,"                     (go (+ j 1) (- u (probs j)))))))"
  ,"    (go 1 (uniform 0 1))))"
  ,"(define (bernoulli p) (if (flip p) 1 0))"
  ]

churchNode :: Label -> Node -> String
churchNode _ (Apply "getExternal" _ _) = ""
churchNode _ (Apply f js _) =
  "("++ fromMaybe f (lookup f churchBuiltinFunctions) ++" "++
    churchNodeRef `spaced` js ++")"
churchNode _ (Array _ (Lambda dag ret) _) =
  "(mem (lambda ("++ churchId `spaced` inputs dag ++") "++
    "(letrec (\n"++ churchDAG dag ++")\n  "++ churchNodeRef ret ++")))"
churchNode name (FoldScan Scan Left_ (Lambda dag ret) seed (Var ls _) _) =
  "(mem (lambda ("++ churchId idx ++") "++
    "(if (= 1 "++ churchId idx ++") "++ churchNodeRef seed ++" "++
      "(letrec (\n  "++
        "("++ churchId i ++" ("++ churchId ls ++" (- "++ churchId idx ++" 1)))\n  "++
        "("++ churchId j ++" ("++ name        ++" (- "++ churchId idx ++" 1)))\n"++
        churchDAG dag ++")\n  "++ churchNodeRef ret ++"))))"
  where d = dagLevel dag
        idx = Dummy d 0
        [i,j] = inputs dag

churchPNode :: PNode -> String
churchPNode (Dist f js _) =
  "("++ fromMaybe f (lookup f churchBuiltinDistributions) ++" "++
    churchNodeRef `spaced` js ++")"
churchPNode (Loop _ (Lambda dag body) _) =
  "(mem (lambda ("++ churchId `spaced` inputs dag ++") "++
    (if churchDAG dag == ""
      then churchPNode body
      else "(letrec (\n"++ churchDAG dag ++")\n  "++ churchPNode body ++")")
    ++"))"

churchDAG :: DAG -> String
churchDAG dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = churchId $ Internal (dagLevel dag) i
      val = churchNode name n
  in if null val then "" else "("++ name ++" "++ val ++")"

churchConstraint :: LVal -> ConstVal -> String
churchConstraint (LVar k) v | dimension v == 1 =
  "(equal? (map "++ churchId k ++" (iota "++ show hi ++" "++ show lo ++")) "++ churchConstVal v ++")"
  where ([lo],[hi]) = bounds v
churchConstraint (LVar k) v = "(equal? "++ churchId k ++" "++ churchConstVal v ++")"

churchResult :: Label -> Type -> String
churchResult name (ArrayT _ [(lo,hi)] _) =
  "(map "++ name ++" (iota "++ churchNodeRef hi ++" "++ churchNodeRef lo ++"))"
churchResult name _ = name

churchProgram :: (ExprTuple t) => Prog t -> String
churchProgram prog
  | Map.null given =
  churchPrelude ++"\n"++
  "(define (model) (letrec (\n"++
    churchDAG (topDAG block) ++"\n"++
    printedRefs ++")\n  "++ printedRets ++"))\n"++
  finalLine
  | otherwise =
  churchPrelude ++"\n"++
  "(mh-query 1000 10 (define p (letrec (\n"++
    churchDAG (topDAG block) ++"\n"++
    printedRefs ++")\n  "++
    "(pair "++ printedRets ++" (and\n"++
    indent (unlines printedConds) ++"))))\n  "++
    "(car p) (cdr p))"
  where (rets, (PBlock block refs given ns)) = runProgExprs "church" prog
        printedRefs = indent . unlines $ zipWith g [0..] (reverse refs)
        g i n = "("++ churchId (Volatile ns 0 i) ++" "++ churchPNode n ++")"
        printedRets | length rets == 1 = churchNodeRef (head rets)
                    | otherwise = "(list "++ churchNodeRef `spaced` rets ++")"
        finalLine | length rets == 1 = churchResult "(model)" $ typeRef (head rets)
                  | otherwise = "(model)"
        printedConds = [churchConstraint k v | (k,(v,t)) <- Map.toList given]

runChurch :: (ExprTuple t) => P t -> IO [String]
runChurch prog = withSystemTempDirectory "church" $ \tmpDir -> do
  pwd <- getCurrentDirectory
  let fname = tmpDir ++"/program.church"
  putStrLn' "--- Generating Church code ---"
  putStrLn' $ churchProgram prog
  fname `writeFile` churchProgram prog
  out <- readProcess (pwd ++"/church.sh") [fname] ""
  let samples = words $ drop 1 $ take (length out - 2) out
  return samples

-- | sample a random vector via Church
simChurchVec :: (Read t, ExprType t) => P (Expression [t]) -> IO [t]
simChurchVec prog = map read <$> runChurch prog

-- | Metropolis-Hastings inference via the Church code generation backend
mhChurch :: (ExprTuple t, Read t) => P t -> IO [t]
mhChurch prog = map read <$> runChurch prog
