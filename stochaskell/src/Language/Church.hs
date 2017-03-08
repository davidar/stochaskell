module Language.Church where

import Data.Expression
import Data.Expression.Const
import Data.List
import Data.Maybe
import Data.Program
import Data.Ratio

type Label = String

spaced :: (a -> String) -> [a] -> String
spaced f = unwords . map f

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

churchId :: Id -> String
churchId (Dummy    level i) = "i_"++ show level ++"_"++ show i
churchId (Volatile level i) = "x_"++ show level ++"_"++ show i
churchId (Internal level i) = "v_"++ show level ++"_"++ show i

churchConstVal :: ConstVal -> String
churchConstVal (Exact a) | isScalar a =
    if d == 1 then show n else show (fromRational c :: Double)
  where c = toScalar a
        n = numerator c
        d = denominator c
churchConstVal (Approx a) | isScalar a = show (toScalar a)

churchNodeRef :: NodeRef -> String
churchNodeRef (Var s _) = churchId s
churchNodeRef (Const c) = churchConstVal c
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
  ]

churchNode :: Label -> Node -> String
churchNode _ (Apply "asVector" [j] _) = churchNodeRef j
churchNode _ (Apply f js _) =
  "("++ fromMaybe f (lookup f churchBuiltinFunctions) ++" "++
    churchNodeRef `spaced` js ++")"
churchNode _ (Array _ dag ret _) =
  "(mem (lambda ("++ churchId `spaced` inputs dag ++") "++
    "(letrec (\n"++ churchDAG dag ++")\n  "++ churchNodeRef ret ++")))"
churchNode name (Scan Left_ dag ret seed (Var ls _) _) =
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
churchPNode (Loop _ dag body _) =
  "(mem (lambda ("++ churchId `spaced` inputs dag ++") "++
    (if churchDAG dag == ""
      then churchPNode body
      else "(letrec (\n"++ churchDAG dag ++")\n  "++ churchPNode body ++")")
    ++"))"

churchDAG :: DAG -> String
churchDAG dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let name = churchId $ Internal (dagLevel dag) i
  in "("++ name ++" "++ churchNode name n ++")"

churchProgram :: (ExprTuple t) => Prog t -> String
churchProgram prog =
  churchPrelude ++"\n"++
  "(define (model) (letrec (\n"++
    churchDAG (head block) ++"\n"++
    printedRefs ++")\n  "++ printedRets ++"))"
  where (rets, (PBlock block refs _)) = runProgExprs prog
        printedRefs = indent . unlines $ zipWith g [0..] (reverse refs)
        g i n = "("++ churchId (Volatile 0 i) ++" "++ churchPNode n ++")"
        printedRets | length rets == 1 = churchNodeRef (head rets)
                    | otherwise = "(list "++ churchNodeRef `spaced` rets ++")"
