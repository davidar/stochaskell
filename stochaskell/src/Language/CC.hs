{-# LANGUAGE TupleSections #-}
module Language.CC where

import Control.Monad
import Control.Monad.State
import Data.Array.Abstract
import qualified Data.ByteString.Char8 as C
import Data.Either.Utils
import Data.Expression hiding (const)
import Data.Expression.Const hiding (isScalar)
import qualified Data.Expression.Const as Const
import Data.Expression.Eval
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Program
import GHC.Exts
import qualified Crypto.Hash.SHA1 as SHA1
import System.Directory
import System.Process
import Util

------------------------------------------------------------------------------
-- UTILITY FUNCTIONS                                                        --
------------------------------------------------------------------------------

ccForLoop :: [Id] -> [Interval NodeRef] -> String -> String
ccForLoop is sh body = concat (zipWith f is sh) ++"{\n"++ body ++"\n}"
  where f i (a,b) = "for(int "++ ccId i ++" = " ++ ccNodeRef a ++"; "++
                                 ccId i ++" <= "++ ccNodeRef b ++"; "++ ccId i ++"++) "
ccForLoop' :: [Id] -> [Interval String] -> String -> String
ccForLoop' is sh body = concat (zipWith f is sh) ++"{\n"++ body ++"\n}"
  where f i (a,b) = "for(int "++ ccId i ++" = " ++ a ++"; "++
                                 ccId i ++" <= "++ b ++"; "++ ccId i ++"++) "


------------------------------------------------------------------------------
-- IDENTIFIERS                                                              --
------------------------------------------------------------------------------

ccId :: Id -> String
ccId = show

ccNodeRef :: NodeRef -> String
ccNodeRef (Var s _) = ccId s
ccNodeRef (Const c _) | dimension c == 0 = show c
ccNodeRef (Index f js) = ccNodeRef f `ccIndex` reverse js
ccNodeRef (Data _ js _) = "make_tuple("++ ccNodeRef `commas` js ++")"
ccNodeRef r = error $ "ccNodeRef "++ show r

ccIndex :: String -> [NodeRef] -> String
ccIndex  a js = a ++ concat ["["++ ccNodeRef j ++"-1]" | j <- js]
ccIndex' :: String -> [Id] -> String
ccIndex' a js = a ++ concat ["["++ ccId      j ++"-1]" | j <- js]


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

ccType :: Type -> String
ccType IntT = "int"
ccType RealT = "double"
ccType (SubrangeT t _ _) = ccType t
ccType (ArrayT _ [_] t) = "vector<"++ ccType t ++">"
ccType (TupleT ts) = "tuple<"++ ccType `commas` ts ++">"
ccType (UnionT ts) = "variant<"++ (ccType . TupleT) `commas` ts ++">"
ccType t = error $ "ccType "++ show t

ccTie :: [Id] -> [Type] -> String
ccTie is ts = concat (zipWith f is ts) ++ "tie("++ ccId `commas` is ++")"
  where f i t = ccType t ++" "++ (ccNodeRef (Var i t)) ++"; "


------------------------------------------------------------------------------
-- NODES                                                                    --
------------------------------------------------------------------------------

ccOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   "*")
  ,("/",   "/")
  ,("==",  "==")
  ,(">",   ">")
  ,(">=",  ">=")
  ,("<",   "<")
  ,("<=",  "<=")
  ]

ccNode :: Map Id PNode -> Label -> Node -> String
ccNode r _ (Apply "getExternal" [Var i _] _) =
  ccPNode (ccId i) . fromJust $ Map.lookup i r
ccNode _ name (Apply "ifThenElse" [a,b,c] _) =
  "if("++ ccNodeRef a ++") "++ name ++" = "++ ccNodeRef b ++"; "++
  "else "++ name ++" = "++ ccNodeRef c ++";"
ccNode _ name (Apply "min" js t) =
  name ++" = min<"++ ccType t ++">("++ ccNodeRef `commas` js ++");"
ccNode _ name (Apply "negate" [i] _) =
  name ++" = -"++ ccNodeRef i ++";"
ccNode _ name (Apply op [i,j] _) | isJust s =
  name ++" = "++ ccNodeRef i ++" "++ fromJust s ++" "++ ccNodeRef j ++";"
  where s = lookup op ccOperators
ccNode _ name (Apply "gamma_lpdf" [i,a,b] _) =
  name ++" = log(boost::math::pdf(boost::math::gamma_distribution<>("++
    ccNodeRef a ++", "++ q ++"), "++ ccNodeRef i ++"));"
  where q = "1./"++ ccNodeRef b
ccNode _ name (Apply "neg_binomial_lpdf" [i,a,b] _) =
  name ++" = log(boost::math::pdf(boost::math::negative_binomial_distribution<>("++
    ccNodeRef a ++", "++ p ++"), "++ ccNodeRef i ++"));"
  where p = ccNodeRef b ++" / ("++ ccNodeRef b ++" + 1)"
ccNode _ name (Apply f (i:js) _) | (d,"_lpdf") <- splitAt (length f - 5) f =
  name ++" = log(boost::math::pdf(boost::math::"++ d ++ "_distribution<>("++
    ccNodeRef `commas` js ++"), "++ ccNodeRef i ++"));"
ccNode _ name (Apply f js _) =
  name ++" = "++ f ++ "("++ ccNodeRef `commas` js ++");"
ccNode _ name (Array sh (Lambda dag ret) _) =
  ccForLoop (inputs dag) sh $
    ccDAG Map.empty dag ++"\n  "++
    name `ccIndex'` inputs dag ++" = "++ ccNodeRef ret ++";"
ccNode _ name (FoldScan fs lr (Lambda dag ret) seed (Var ls s) t) =
  name ++ sloc ++" = "++ ccNodeRef seed ++";\n"++
  ccForLoop' [idx] [("1",n)] (unlines
    ["  "++ ccType (typeIndex 1 s) ++" "++ ccId i ++" = "++ ccId ls ++"["++ loc ++"];"
    ,"  "++ ccType (if fs == Fold then t else typeIndex 1 t) ++" "++
              ccId j ++" = "++ name ++ ploc ++";"
    ,       ccDAG Map.empty dag
    ,"  "++ name ++ rloc ++" = "++ ccNodeRef ret ++";"
    ])
  where d = dagLevel dag
        idx = Dummy d 0
        [i,j] = inputs dag
        n = ccId ls ++".size()"
        loc = case lr of
          Left_  -> ccId idx ++"-1"
          Right_ -> n ++"-"++ ccId idx
        sloc = case fs of
          Fold -> ""
          Scan -> case lr of
            Left_ -> "[0]"
            Right_ -> "["++ n ++"]"
        (ploc,rloc) = case fs of
          Fold -> ("","")
          Scan -> case lr of
            Left_  -> ("["++ loc ++"-1]", "["++ loc ++"]")
            Right_ -> ("["++ loc ++"]", "["++ loc ++"-1]")
ccNode _ _ node = error $ "unable to codegen node "++ show node

ccPNode :: Label -> PNode -> String
ccPNode name (Dist "bernoulli" [p] _) =
  name ++" = std::bernoulli_distribution("++ ccNodeRef p ++")(gen);"
ccPNode name (Dist "gamma" [a,b] _) =
  name ++" = std::gamma_distribution<>("++ ccNodeRef a ++", "++ q ++")(gen);"
  where q = "1./"++ ccNodeRef b
ccPNode name (Dist f args _) =
  name ++" = std::"++ f ++"_distribution<>("++ ccNodeRef `commas` args ++")(gen);"
ccPNode name (Loop sh (Lambda ldag body) _) =
  ccForLoop (inputs ldag) sh $
    let lval = name `ccIndex'` inputs ldag
    in ccDAG Map.empty ldag ++ indent (ccPNode lval body)
ccPNode name (Switch e alts ns _) =
  "switch("++ ccNodeRef e ++".index()) {\n"++ indent (unlines $ do
    (i, (Lambda dag ret, refs)) <- zip [0..] alts
    ["case "++ show i ++": {",
     "  "++ ccTie (inputs dag) (tss!!i) ++" = get<"++ show i ++">("++ ccNodeRef e ++");",
            ccDAG (pnodes' ns (dagLevel dag) refs) dag,
     "  "++ name ++" = "++ f ret ++"; break;",
     "}"]) ++"\n}"
  where UnionT tss = typeRef e
        f [j] = ccNodeRef j
        f js = "make_tuple("++ ccNodeRef `commas` js ++")"

ccDAG :: Map Id PNode -> DAG -> String
ccDAG r dag = indent. unlines . flip map (nodes dag) $ \(i,n) ->
  let name = ccId $ Internal (dagLevel dag) i
  in decl name n ++ ccNode r name n
  where decl _ (Apply "getExternal" [i] t) = ccType t ++" "++ (ccNodeRef i) ++"; "
        decl name node = ccType (typeNode node) ++" "++ name ++"; "


------------------------------------------------------------------------------
-- WHOLE PROGRAMS                                                           --
------------------------------------------------------------------------------

ccRead :: Int -> NodeRef -> String
ccRead l e = ccType (typeRef e) ++" "++ ccNodeRef e ++"; "++ ccRead' l e
ccRead' :: Int -> NodeRef -> String
ccRead' _ e | isScalar (typeRef e) = "cin >> "++ ccNodeRef e ++";"
ccRead' l e | (ArrayT _ [_] t) <- typeRef e =
  "int "++ n ++"; cin >> "++ n ++";\n"++
  ccNodeRef e ++".resize("++ n ++");\n"++
  (ccForLoop' is [("1",n)] $ "  cin >> "++ ccNodeRef e `ccIndex'` is ++";")
  where n = ccNodeRef e ++"_length"
        is = [Dummy l 1]
ccRead' l e | UnionT tss <- typeRef e =
  ccRead (l+1) c ++"\n"++
  "switch("++ ccNodeRef c ++") {\n"++ indent (unlines $ do
    i  <- [0..length tss - 1]
    let ts = tss !! i
        js = Dummy l <$> [1..length ts]
    ["case "++ show i ++": {",
     indent . unlines $ zipWith ((ccRead (l+1) .). Var) js ts,
     "  "++ ccNodeRef e ++" = make_tuple("++ ccId `commas` js ++");",
     "  break;",
     "}"]) ++"\n}"
  where c = Var (Dummy l 0) IntT

ccPrint :: Int -> NodeRef -> String
ccPrint _ e | isScalar (typeRef e) = "cout << "++ ccNodeRef e ++" << ' ';"
ccPrint l e | (ArrayT _ [_] _) <- typeRef e =
  "cout << "++ n ++" << ' ';\n"++
  (ccForLoop' is [("1",n)] $ "  cout << "++ ccNodeRef e `ccIndex'` is ++" << ' ';")
  where n = ccNodeRef e ++".size()"
        is = [Dummy l 1]
ccPrint l e | UnionT tss <- typeRef e =
  "switch("++ ccNodeRef e ++".index()) {\n"++ indent (unlines $ do
    i  <- [0..length tss - 1]
    let ts = tss !! i
        js = Dummy l <$> [1..length ts]
    ["case "++ show i ++": {",
     "  "++ ccTie js ts ++" = get<"++ show i ++">("++ ccNodeRef e ++");",
     "  cout << "++ show i ++" << ' ';",
     indent . unlines $ zipWith ((ccPrint (l+1) .). Var) js ts,
     "  break;",
     "}"]) ++"\n}"

ccProgram :: (ExprTuple s, ExprTuple t) => State Block [Type] -> (s -> Prog t) -> (String, [NodeRef])
ccProgram s prog = (,rets) $ unlines
  ["#include <cmath>"
  ,"#include <iostream>"
  ,"#include <random>"
  ,"#include <tuple>"
  ,"#include <boost/math/distributions.hpp>"
  ,"#include <mpark/variant.hpp>"
  ,"using namespace std;"
  ,"using namespace mpark;"
  ,""
  ,"int main() {"
  ,"  random_device rd;"
  ,"  mt19937 gen(rd());"
  ,   indent $ unlines [ccRead 1 $ Var (Dummy 0 i) t | (i,t) <- zip [0..] ts]
  ,   ccDAG (pnodes pb) (topDAG block)
  ,   indent $ unlines [ccPrint 1 ret ++" cout << endl;" | ret <- rets]
  ,"}"
  ]
  where (rets, pb) = runProgExprs "cc" . prog $ toExprTuple
          [DExpr . return $ Var (Dummy 0 i) t | (i,t) <- zip [0..] ts]
        (ts, block) = runState s $ definitions pb

printCC :: ConstVal -> String
printCC (Tagged c ks) = unwords $ show c : map printCC ks
printCC c | Const.isScalar c = show c
          | otherwise = let cs = toList c in
              unwords $ show (length cs) : map printCC cs

readChain :: [s -> (c, s)] -> s -> ([c], s)
readChain [] s = ([],s)
readChain (f:fs) s = (c:cs, s'')
  where (c,s') = f s
        (cs,s'') = readChain fs s'

readCC :: Type -> [String] -> (ConstVal, [String])
readCC IntT (x:s) = (fromInteger $ read x, s)
readCC RealT (x:s) = (fromDouble $ read x, s)
readCC (SubrangeT t _ _) s = readCC t s
readCC (ArrayT _ [_] t) (x:s) = (fromList cs, s')
  where n = fromInteger $ read x
        (cs,s') = readChain (replicate n $ readCC t) s
readCC (UnionT tss) (x:s) = (Tagged c cs, s')
  where c = read x
        ts = tss !! c
        (cs,s') = readChain (map readCC ts) s

runCC :: (ExprTuple s, ExprTuple t) => (s -> Prog t) -> s -> IO t
runCC prog x = do
  pwd <- getCurrentDirectory
  let hash = toHex . SHA1.hash $ C.pack code
      exename = pwd ++"/cache/cc/sampler_"++ hash
      srcname = exename ++".cc"
  exeExists <- doesFileExist exename

  unless exeExists $ do
    putStrLn' code
    writeFile srcname code
    let cxx = "g++"
        args = ["-std=c++11", "-Ivariant/include", "-o", exename, srcname]
    putStrLn' $ unwords (cxx:args)
    callProcess cxx args

  let input = fromRight' (evalTuple emptyEnv x)
  output <- readProcess exename [] . unlines $ printCC <$> input
  let (cs,[]) = readChain (readCC . typeRef <$> rets) $ words output
  return $ fromConstVals cs
  where (code,rets) = ccProgram (typeExprTuple x) prog
