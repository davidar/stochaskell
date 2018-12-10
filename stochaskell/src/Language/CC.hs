{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
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
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Program
import Data.String.Utils
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
ccForLoop' :: [String] -> [Interval String] -> String -> String
ccForLoop' is sh body = concat (zipWith f is sh) ++"{\n"++ body ++"\n}"
  where f i (a,b) = "for(int "++ i ++" = " ++ a ++"; "++
                                 i ++" <= "++ b ++"; "++ i ++"++) "


------------------------------------------------------------------------------
-- IDENTIFIERS                                                              --
------------------------------------------------------------------------------

ccId :: Id -> String
ccId = show

ccNodeRef :: NodeRef -> String
ccNodeRef (Var s _) = ccId s
ccNodeRef (Const c _) | dimension c == 0 = show c
ccNodeRef (Const c _) | dimension c == 1 = "{"++ show `commas` toList c ++"}"
ccNodeRef (Index f js) = ccNodeRef f `ccIndex` reverse js
ccNodeRef (Data _ js _) = "make_tuple("++ ccNodeRef `commas` js ++")"
ccNodeRef (Extract r 0 i) = "get<"++ show i ++">("++ ccNodeRef r ++")"
ccNodeRef (Cond cvs _)
  | replicated vs = ccNodeRef $ unreplicate vs
  | otherwise = intercalate " : " $
    [ccNodeRef c ++" ? "++ ccNodeRef v | (c,v) <- init cvs]
    ++ [ccNodeRef . snd $ last cvs]
  where vs = snd <$> cvs
ccNodeRef r = error $ "ccNodeRef "++ show r

ccIndex :: String -> [NodeRef] -> String
ccIndex a js = a ++"("++ intercalate ", " [ccNodeRef j ++"-1" | j <- js] ++")"
ccIndex' :: String -> [String] -> String
ccIndex' a js = a ++"("++ intercalate ", " [j ++"-1" | j <- js] ++")"


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

ccType :: Type -> String
ccType IntT = "int"
ccType RealT = "double"
ccType (SubrangeT t _ _) = ccType t
ccType (ArrayT (Just "vector") _ t) = "Matrix<"++ ccType t ++", Dynamic, 1>"
ccType (ArrayT (Just "matrix") _ t) = "Matrix<"++ ccType t ++", Dynamic, Dynamic>"
ccType (TupleT []) = "void"
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
  ,("<>",  "*")
  ,("#>",  "*")
  ,("==",  "==")
  ,("/=",  "!=")
  ,(">",   ">")
  ,(">=",  ">=")
  ,("<",   "<")
  ,("<=",  "<=")
  ,("&&",  "&&")
  ,("||",  "||")
  ]

ccDebug :: String -> [NodeRef] -> String
ccDebug msg js = "cerr << \"[DEBUG] "++ msg ++"\" << "++ s ++" << endl;"
  where s = case js of
          [j] -> g j
          _ -> printTuple js
        g j = case typeRef j of
          ArrayT{}  -> ccNodeRef j ++".format(eigenFormat)"
          TupleT [] -> "\"()\""
          TupleT ts -> printTuple [Extract j 0 i | (i,t) <- zip [0..] ts]
          _ -> ccNodeRef j
        printTuple js =
          "'(' << "++ intercalate " << ',' << " (g <$> js) ++" << ')'"

ccNode :: Map Id PNode -> Label -> Node -> String
ccNode r _ (Apply "getExternal" [Var i _] _) =
  ccPNode' (ccId i) . fromJust $ Map.lookup i r
ccNode _ _ (Apply f js _) | ("debug$",msg) <- splitAt 6 f = ccDebug (msg ++": ") js
ccNode _ name (Apply "id" [i] _) =
  name ++" = "++ ccNodeRef i ++";"
ccNode _ name (Apply "&&s" js _) =
  name ++" = "++ intercalate " && " (ccNodeRef <$> js) ++";"
ccNode _ name (Apply "+s" js _) =
  name ++" = "++ intercalate " + " (ccNodeRef <$> js) ++";"
ccNode _ name (Apply "==" [u,v] _) | ArrayT{} <- typeRef u =
  name ++" = "++ ccNodeRef u ++".rows() == "++ ccNodeRef v ++".rows() && "++
                 ccNodeRef u ++".cols() == "++ ccNodeRef v ++".cols() && "++
                 ccNodeRef u ++" == "++ ccNodeRef v ++";"
ccNode _ name (Apply "ifThenElse" [a,b,c] _) =
  "if("++ ccNodeRef a ++") "++ name ++" = "++ ccNodeRef b ++"; "++
  "else "++ name ++" = "++ ccNodeRef c ++";"
ccNode _ name (Apply "vectorSize" [v] _) =
  name ++" = "++ ccNodeRef v ++".size();"
ccNode _ name (Apply "eye" [i] t) =
  name ++" = "++ ccType t ++"::Identity("++ ccNodeRef i ++", "++ ccNodeRef i ++");"
ccNode _ name (Apply "min" js t) =
  name ++" = min<"++ ccType t ++">("++ ccNodeRef `commas` js ++");"
ccNode _ name (Apply "negate" [i] _) =
  name ++" = -"++ ccNodeRef i ++";"
ccNode _ name (Apply op [i,j] _) | isJust s =
  name ++" = "++ ccNodeRef i ++" "++ fromJust s ++" "++ ccNodeRef j ++";"
  where s = lookup op ccOperators
ccNode _ name (Apply "<.>" [u,v] _) =
  name ++" = "++ ccNodeRef u ++".dot("++ ccNodeRef v ++");"
ccNode _ name (Apply "<\\>" [a,b] _) =
  "if("++ ccNodeRef a ++".isLowerTriangular()) "++
  name ++" = "++ ccNodeRef a ++".triangularView<Lower>().solve("++ ccNodeRef b ++");"++
  " else "++ name ++" = "++ ccNodeRef a ++".llt().solve("++ ccNodeRef b ++");"
ccNode _ name (Apply "chol" [m] _) =
  name ++" = "++ ccNodeRef m ++".llt().matrixL();"
ccNode _ name (Apply "inv" [m] _) =
  name ++" = "++ ccNodeRef m ++".inverse();"
ccNode _ name (Apply "log_det" [m] _) =
  name ++" = LOG_DET("++ ccNodeRef m ++");"
ccNode _ name (Apply "logFactorial" [i] _) =
  name ++" = boost::math::lgamma<double>(1 + "++ ccNodeRef i ++");"
ccNode _ name (Apply "replaceIndex" [v,j,e] _) =
  name ++" = "++ ccNodeRef v ++"; "++ name `ccIndex` [j] ++" = "++ ccNodeRef e ++";"
ccNode _ name (Apply "insertIndex" [v,j,e] _) =
  name ++".resize("++ n ++");\n"++
  (ccForLoop' [i] [("1",n)] $
    "  if("++ i ++" < "++ ccNodeRef j ++") "++
      name `ccIndex'` [i] ++" = "++ ccNodeRef v `ccIndex'` [i] ++";\n"++
    "  else if("++ i ++" == "++ ccNodeRef j ++") "++
      name `ccIndex'` [i] ++" = "++ ccNodeRef e ++";\n"++
    "  else "++
      name `ccIndex'` [i] ++" = "++ ccNodeRef v `ccIndex'` [i ++"-1"] ++";")
  where i = "i_"++ name
        n = "("++ ccNodeRef v ++".size() + 1)"
ccNode _ name (Apply "deleteIndex" [v,j] _) =
  name ++".resize("++ n ++");\n"++
  (ccForLoop' [i] [("1",n)] $
    "  if("++ i ++" < "++ ccNodeRef j ++") "++
      name `ccIndex'` [i] ++" = "++ ccNodeRef v `ccIndex'` [i] ++";\n"++
    "  else "++
      name `ccIndex'` [i] ++" = "++ ccNodeRef v `ccIndex'` [i ++"+1"] ++";")
  where i = "i_"++ name
        n = "("++ ccNodeRef v ++".size() - 1)"
ccNode _ name (Apply "findSortedInsertIndex" [e,v] _) =
  name ++" = "++ n ++" + 1;\n"++
  (ccForLoop' [j] [("1",n)] $
    "  if("++ ccNodeRef e ++" < "++ ccNodeRef v `ccIndex'` [j] ++") { "++
      name ++" = "++ j ++"; break; }")
  where j = "i_"++ name
        n = ccNodeRef v ++".size()"
ccNode _ name (Apply "bernoulliLogit_lpdf" [i,l] _) =
  name ++" = log("++ ccNodeRef i ++" ? "++ p ++" : 1. - "++ p ++");"
  where p = "1./(1. + exp(-"++ ccNodeRef l ++"))"
ccNode _ name (Apply "discreteUniform_lpdf" [i,a,b] _) =
  name ++" = log("++ c ++" ? "++ p ++" : 0.);"
  where c = "("++ ccNodeRef a ++" <= "++ ccNodeRef i ++" && "++
                  ccNodeRef i ++" <= "++ ccNodeRef b ++")"
        p = "1./("++ ccNodeRef b ++" - "++ ccNodeRef a ++" + 1)"
ccNode _ name (Apply "gamma_lpdf" [i,a,b] _) =
  name ++" = log(boost::math::pdf(boost::math::gamma_distribution<>("++
    ccNodeRef a ++", "++ q ++"), "++ ccNodeRef i ++"));"
  where q = "1./"++ ccNodeRef b
ccNode _ name (Apply "neg_binomial_lpdf" [i,a,b] _) =
  name ++" = log(boost::math::pdf(boost::math::negative_binomial_distribution<>("++
    ccNodeRef a ++", "++ p ++"), "++ ccNodeRef i ++"));"
  where p = ccNodeRef b ++" / ("++ ccNodeRef b ++" + 1)"
ccNode _ name (Apply "normals_lpdf" [x,m,s] _) =
  name ++" = 0.; "++ (ccForLoop' i [("1",n)] $ "  "++
    name ++" += log(boost::math::pdf(boost::math::normal_distribution<>("++
      ccNodeRef m `ccIndex'` i ++", "++ ccNodeRef s `ccIndex'` i ++"), "++
      ccNodeRef x `ccIndex'` i ++"));")
  where i = ["i_"++ name]
        n = ccNodeRef x ++".size()"
ccNode _ name (Apply "multi_normal_lpdf" [z,m,c] _) =
  name ++" = -0.5 * ("++
    ccNodeRef c ++".llt().matrixL().solve("++ z' ++").squaredNorm()"++
    " + log(2*M_PI) * "++ ccNodeRef z ++".size() + LOG_DET("++ ccNodeRef c ++"));"
  where z' = "("++ ccNodeRef z ++" - "++ ccNodeRef m ++")"
ccNode _ name (Apply f (i:js) _) | (d,"_lpdf") <- splitAt (length f - 5) f =
  name ++" = log(boost::math::pdf(boost::math::"++ d ++ "_distribution<>("++
    ccNodeRef `commas` js ++"), "++ ccNodeRef i ++"));"
ccNode _ name (Apply f js _) =
  name ++" = "++ f ++ "("++ ccNodeRef `commas` js ++");"
ccNode _ name (Array sh (Lambda dag ret) _) =
  name ++".resize("++ (ccNodeRef . snd) `commas` sh ++");\n"++
  (ccForLoop (inputs dag) sh $
    ccDAG Map.empty dag ++"\n  "++
    name `ccIndex'` (ccId <$> inputs dag) ++" = "++ ccNodeRef ret ++";")
ccNode _ name (FoldScan fs lr (Lambda dag ret) seed ls t) =
  name ++ sloc ++" = "++ ccNodeRef seed ++";\n"++
  ccForLoop' [ccId idx] [("1",n)] (unlines
    ["  "++ ccType (typeIndex 1 s) ++" "++ ccId i ++" = "++ ccNodeRef ls ++"["++ loc ++"];"
    ,"  "++ ccType (if fs == Fold then t else typeIndex 1 t) ++" "++
              ccId j ++" = "++ name ++ ploc ++";"
    ,       ccDAG Map.empty dag
    ,"  "++ name ++ rloc ++" = "++ ccNodeRef ret ++";"
    ])
  where s = typeRef ls
        d = dagLevel dag
        idx = Dummy d 0
        [i,j] = inputs dag
        n = ccNodeRef ls ++".size()"
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
ccNode _ name (Case e alts t) =
  "switch("++ ccNodeRef e ++") {\n"++ cases ++"\n}"
  where cases = unlines $ do
          (i, Lambda dag ret) <- zip [0..] alts
          let lhs | typeRef e == IntT = show (i+1)
              rhs = unlines
                [ccDAG Map.empty dag
                ,name ++" = "++ ccNodeRef (Data 0 ret t) ++";"
                ,"break;"]
          return $ "case "++ lhs ++":\n"++ indent rhs
ccNode _ _ node = error $ "ccNode "++ show node

ccDistributions =
  [("discreteUniform", "uniform_int")
  ,("normal",          "normal")
  ,("poisson",         "poisson")
  ,("uniform",         "uniform_real")]

ccPNode' :: Label -> PNode -> String
ccPNode' name pn | (SubrangeT _ a b) <- typePNode pn =
  "do { "++ ccPNode name pn ++" } while("++
    maybe "false" (\a -> name ++" < "++ ccNodeRef a) a ++" || "++
    maybe "false" (\b -> name ++" > "++ ccNodeRef b) b ++");"
ccPNode' name pn = ccPNode name pn

ccPNode :: Label -> PNode -> String
ccPNode name (Dist "bernoulli" [p] _) =
  name ++" = std::bernoulli_distribution("++ ccNodeRef p ++")(gen);"
ccPNode name (Dist "categorical" [ps] _) =
  name ++" = 1 + std::discrete_distribution<>("++ ccNodeRef ps ++")(gen);"
ccPNode name (Dist "gamma" [a,b] _) =
  name ++" = std::gamma_distribution<>("++ ccNodeRef a ++", "++ q ++")(gen);"
  where q = "1./"++ ccNodeRef b
ccPNode name (Dist f args _) | Just s <- lookup f ccDistributions =
  name ++" = std::"++ s ++"_distribution<>("++ ccNodeRef `commas` args ++")(gen);"
ccPNode name (Loop sh (Lambda ldag body) _) =
  ccForLoop (inputs ldag) sh $
    let lval = name `ccIndex'` (ccId <$> inputs ldag)
    in ccDAG Map.empty ldag ++ indent (ccPNode' lval body)
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
ccPNode name (Chain (lo,hi) refs (Lambda dag ret) x ns _) =
  name ++" = "++ ccNodeRef x ++";\n"++
  (ccForLoop' [ccNodeRef i] [(ccNodeRef lo, ccNodeRef hi)] $
    "  "++ ccType (typeRef j) ++" "++ ccNodeRef j ++" = "++ name ++";\n"++
    ccDAG (pnodes' ns (dagLevel dag) refs) dag ++"\n"++
    "  "++ name ++" = "++ ccNodeRef ret ++";")
  where [i,j] = inputs' dag
ccPNode _ pn = error $ "ccPNode "++ show pn

debugRef :: NodeRef -> Bool
--debugRef = isScalar . typeRef
debugRef = const False

ccDAG :: Map Id PNode -> DAG -> String
ccDAG r dag = indent . unlines . flip map (nodes dag) $ \(i,n) ->
  let ref = case n of
        Apply "getExternal" [v] _ -> v
        _ -> Var (Internal (dagLevel dag) i) (typeNode n)
      name = ccNodeRef ref
      msg = name ++" = "++ (replace "\n" "\\n" . replace "\\" "\\\\" $ show n) ++" = "
      wrapTry s 
        | dagLevel dag == 0 || length (nodes dag) > 10 =
          "try { "++
            s ++" "++ (if debugRef ref then ccDebug msg [ref] ++" " else "") ++
            "} catch(const std::runtime_error& e) "++
            "{ cerr << \""++ name ++": \" << e.what() << endl; }"
        | otherwise = s
  in decl name n ++ wrapTry (ccNode r name n)
  where decl name node = case node of
          Apply "getExternal" [i] _ ->
            ccType (typeRef i) ++" "++ (ccNodeRef i) ++"; "
          Apply _ _ (TupleT []) -> ""
          _ -> ccType (typeNode node) ++" "++ name ++"; "


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
  (ccForLoop' (ccId <$> is) [("1",n)] $
    "  cin >> "++ ccNodeRef e `ccIndex'` (ccId <$> is) ++";")
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
  (ccForLoop' (ccId <$> is) [("1",n)] $
    "  cout << "++ ccNodeRef e `ccIndex'` (ccId <$> is) ++" << ' ';")
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

ccProgram :: (ExprTuple s, ExprTuple t) => [Type] -> (s -> Prog t) -> (String, [NodeRef])
ccProgram ts prog = (,rets) $ unlines
  ["/*"
  ,showPBlock pb $ show rets
  ,"*/"
  ,""
  ,"#include <cmath>"
  ,"#include <iostream>"
  ,"#include <random>"
  ,"#include <tuple>"
  ,"#include <boost/math/distributions.hpp>"
  ,"#include <boost/math/special_functions/factorials.hpp>"
  ,"#define eigen_assert(X) do { if(!(X)) throw std::runtime_error(#X); } while(false);"
  ,"#include <Eigen/Dense>"
  ,"#include <mpark/variant.hpp>"
  ,"#include <backward.hpp>"
  ,"using namespace std;"
  ,"using namespace Eigen;"
  ,"using namespace mpark;"
  ,"namespace backward { backward::SignalHandling sh; }"
  ,"IOFormat eigenFormat(StreamPrecision, DontAlignCols, "++
    "\",\", \";\", \"\", \"\", \"[\", \"]\");"
  ,"#define LOG_DET(m) ((m).isLowerTriangular() ? (m).diagonal().array().log().sum()"++
              " : (m).llt().matrixL().toDenseMatrix().diagonal().array().log().sum() * 2)"
  ,""
  ,"int main() {"
  ,"  random_device rd;"
  ,"  mt19937 gen(rd());"
  ,   indent $ unlines [ccRead 1 $ Var (Dummy 0 i) t | (i,t) <- zip [0..] ts]
  ,   ccDAG (pnodes pb) (topDAG $ definitions pb)
  ,   indent $ unlines [ccPrint 1 ret ++" cout << endl;" | ret <- rets]
  ,"}"
  ]
  where (rets, pb) = runProgExprs "cc" . prog $ toExprTuple
          [DExpr . return $ Var (Dummy 0 i) t | (i,t) <- zip [0..] ts]

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

runCC :: forall s t. (ExprTuple s, ExprTuple t) => (s -> Prog t) -> s -> IO t
runCC prog x = do
  p <- compileCC prog
  p x

compileCC :: forall s t. (ExprTuple s, ExprTuple t)
          => (s -> Prog t) -> IO (s -> IO t)
compileCC prog = do
  let TypesIs ts = typesOf :: TypesOf s
      (code,rets) = ccProgram ts prog

  pwd <- getCurrentDirectory
  let hash = toHex . SHA1.hash $ C.pack code
      exename = pwd ++"/cache/cc/sampler_"++ hash
      srcname = exename ++".cc"
  exeExists <- doesFileExist exename

  unless exeExists $ do
    putStrLn' code
    writeFile srcname code
    let cxx = "g++"
        args = ["-std=c++11"
               ,"-Ibackward-cpp", "-Ieigen", "-Ivariant/include"
               ,"-DBACKWARD_HAS_BFD=1"
               ,"-g", "-rdynamic"
               ,srcname
               ,"-lbfd", "-ldl"
               ,"-o", exename
               ]
    putStrLn' $ unwords (cxx:args)
    callProcess cxx args

  return $ \x -> do
    let input = fromRight' (evalTuple emptyEnv x)
    output <- readProcess exename [] . unlines $ printCC <$> input
    let (cs,[]) = readChain (readCC . typeRef <$> rets) $ words output
    return $ fromConstVals cs
