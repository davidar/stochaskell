{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Stan where

import Control.Applicative ()
import Control.Monad
import qualified Data.Array.IArray as A
import Data.Array.Abstract hiding ((<>))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Either.Utils
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Expression.Eval
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Program
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import qualified Crypto.Hash.SHA1 as SHA1
import System.Directory
import System.IO
import System.IO.Temp
import System.Process
import Util

-- TODO: better handling for exceptions to 1-based indexing

------------------------------------------------------------------------------
-- UTILITY FUNCTIONS                                                        --
------------------------------------------------------------------------------

forLoop :: [String] -> [Interval NodeRef] -> String -> String
forLoop is sh body = concat (zipWith f is sh) ++"{\n"++ body ++"\n}"
    where f i (a,b) =
            "for ("++ i ++" in "++ stanNodeRef a
                           ++":"++ stanNodeRef b ++") "


------------------------------------------------------------------------------
-- IDENTIFIERS                                                              --
------------------------------------------------------------------------------

stanId :: Id -> String
stanId (Dummy    level i) = "i_"++ show level ++"_"++ show i
stanId (Volatile ns level i) = "x_"++ ns ++"_"++ show level ++"_"++ show i
stanId (Internal level i) = "v_"++ show level ++"_"++ show i

stanId' :: String -> Either String Id
stanId' i@(t:'_':s) = case t of
  'i' -> let [l,p] =    splitOn "_" s in Right (Dummy       (read l) (read p))
  'x' -> let [ns,l,p] = splitOn "_" s in Right (Volatile ns (read l) (read p))
  'v' -> let [l,p] =    splitOn "_" s in Right (Internal    (read l) (read p))
  _ -> Left i
stanId' i = Left i

stanNodeRef :: NodeRef -> String
stanNodeRef (Var s _) = stanId s
stanNodeRef (Const c _) | dimension c == 0 = show c
stanNodeRef (Index f js) =
    stanNodeRef f ++"["++ stanNodeRef `commas` reverse js ++"]"
stanNodeRef a | isBlockMatrix a =
  foldl1 appendRow [foldl1 appendCol (stanNodeRef <$> row) | row <- fromBlockMatrix a]
  where appendCol a b = "append_col("++ a ++", "++ b ++")"
        appendRow a b = "append_row("++ a ++", "++ b ++")"
stanNodeRef r = error $ "stanNodeRef "++ show r


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

stanType :: Bool -> Type -> String
stanType _ IntT = "int"
stanType _ RealT = "real"
stanType False (SubrangeT t _ _) = stanType False t
stanType True (SubrangeT t Nothing  Nothing ) =
  stanType False t
stanType True (SubrangeT t (Just l) Nothing ) =
  stanType False t ++"<lower="++ stanNodeRef l ++">"
stanType True (SubrangeT t Nothing  (Just h)) =
  stanType False t ++"<upper="++ stanNodeRef h ++">"
stanType True (SubrangeT t (Just l) (Just h)) =
  stanType False t ++"<lower="++ stanNodeRef l ++
                     ",upper="++ stanNodeRef h ++">"
stanType _ (ArrayT _ [(Const 1 _,n)] RealT) =
  "vector["++ stanNodeRef n ++"]"
stanType _ (ArrayT (Just k) [(Const 1 _,m), (Const 1 _,n)] RealT) |
  k `elem` ["cov_matrix", "corr_matrix"], m == n =
  k ++"["++ stanNodeRef m ++"]"
stanType _ (ArrayT _ [(Const 1 _,m), (Const 1 _,n)] RealT) =
  "matrix["++ stanNodeRef m ++","++ stanNodeRef n ++"]"
stanType showRange (ArrayT kind n t) =
    fromMaybe (stanType showRange t) kind ++
      "["++ stanNodeRef `commas` map snd n ++"]"

stanDecl :: Bool -> Label -> Type -> String
stanDecl showRange name (ArrayT _ n t)
  | length n > 2 || t /= RealT =
    stanType showRange t ++" "++ name ++
      "["++ stanNodeRef `commas` map snd n ++"];"
stanDecl showRange name t = stanType showRange t ++" "++ name ++";"


------------------------------------------------------------------------------
-- NODES                                                                    --
------------------------------------------------------------------------------

stanBuiltinFunctions =
  [("chol",        ["cholesky_decompose", "to_matrix"])
  ,("det",         ["determinant",        "to_matrix"])
  ,("inv",         ["inverse",            "to_matrix"])
  ,("negate",      ["-"])
  ,("sqrt",        ["sqrt"])
  ,("exp",         ["exp"])
  ,("log",         ["log"])
  ,("cos",         ["cos"])
  ,("sin",         ["sin"])
  ,("tan",         ["tan"])
  ,("diag",        ["diag_matrix"])
  ,("asColumn",    ["to_matrix", "to_vector"])
  ,("asRow",       ["to_matrix", "to_row_vector"])
  ,("quad_form_diag", ["quad_form_diag"])
  ]

stanVectorisedDistributions =
  [("bernoulliLogits", "bernoulli_logit")
  ,("normals",         "normal")
  ,("uniforms",        "uniform")
  ]

stanBuiltinDistributions =
  [("bernoulliLogit",  "bernoulli_logit")
  ,("cauchy",          "cauchy")
  ,("gamma",           "gamma")
  ,("inv_gamma",       "inv_gamma")
  ,("inv_wishart",     "inv_wishart")
  ,("lkj_corr",        "lkj_corr")
  ,("multi_normal",    "multi_normal")
  ,("neg_binomial",    "neg_binomial")
  ,("normal",          "normal")
  ,("poisson",         "poisson")
  ,("uniform",         "uniform")
  ,("wishart",         "wishart")
  ] ++ stanVectorisedDistributions

stanOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   ".*")
  ,("*>",  "*")
  ,("/",   "./")
  ,("==",  "==")
  ,("/=",  "!=")
  ,("<",   "<")
  ,("<=",  "<=")
  ,(">",   ">")
  ,(">=",  ">=")
  ,("**",  "^")
  ]

stanMatrixOperators =
  [("#>",  ("to_matrix", "*", "to_vector"))
  ,("<>",  ("to_matrix", "*", "to_matrix"))
  ]

stanCompose :: [String] -> String -> String
stanCompose [] x = x
stanCompose (f:fs) x = f ++"("++ stanCompose fs x ++")"

stanNode :: Label -> Node -> String
stanNode _ (Apply "getExternal" _ _) = ""
stanNode name (Apply "id" [a] _) =
    name ++" = "++ stanNodeRef a ++";"
stanNode name (Apply "ifThenElse" [a,b,c] _) =
    name ++" = "++ stanNodeRef a ++" ? "++ stanNodeRef b ++" : "++ stanNodeRef c ++";"
stanNode name (Apply "tr'" [a] _) =
    name ++" = "++ stanNodeRef a ++"';"
stanNode name (Apply "zeros" [m,n] _) =
    name ++" = rep_matrix(0, "++ stanNodeRef m ++", "++ stanNodeRef n ++");"
stanNode name (Apply "findSortedInsertIndex" [x,v] _) =
    name ++" = "++ stanNodeRef n ++" + 1;\n"++
    (forLoop [i] sh $
      "if("++ stanNodeRef x ++" < "++ stanNodeRef v ++"["++ i ++"]) { "++
        name ++" = "++ i ++"; break; }")
    where ArrayT _ sh@[(Const 1 IntT,n)] _ = typeRef v
          i = name ++"_i"
stanNode name (Apply op [i,j] _)
  | isJust $ lookup op stanOperators,
    (ArrayT _ [(Const 1 IntT, Const m IntT), (Const 1 IntT, Const 1 IntT)] _) <- typeRef i,
    (ArrayT _ [(Const 1 IntT, Const 1 IntT), (Const 1 IntT, Const n IntT)] _) <- typeRef j =
    name ++" = rep_matrix(to_vector("++ stanNodeRef i ++"), "++ show n ++") "++
     s ++" rep_matrix(to_row_vector("++ stanNodeRef j ++"), "++ show m ++");"
  | isJust $ lookup op stanOperators,
    (ArrayT _ [(Const 1 IntT, Const m IntT), (Const 1 IntT, Const 1 IntT)] _) <- typeRef i,
    (ArrayT _ [(Const 1 IntT, Const k IntT), (Const 1 IntT, Const n IntT)] _) <- typeRef j,
    m == k =
    name ++" = rep_matrix(to_vector("++ stanNodeRef i ++"), "++ show n ++") "++ s ++" "++ stanNodeRef j ++";"
  | isJust $ lookup op stanOperators,
    (ArrayT _ [(Const 1 IntT, Const 1 IntT), (Const 1 IntT, Const n IntT)] _) <- typeRef i,
    (ArrayT _ [(Const 1 IntT, Const m IntT), (Const 1 IntT, Const k IntT)] _) <- typeRef j,
    n == k =
    name ++" = rep_matrix(to_row_vector("++ stanNodeRef i ++"), "++ show m ++") "++ s ++" "++ stanNodeRef j ++";"
  | isJust $ lookup op stanOperators =
    name ++" = "++ stanNodeRef i ++" "++ s ++" "++ stanNodeRef j ++";"
  where s = fromJust $ lookup op stanOperators
stanNode name (Apply op [i,j] _) | isJust $ lookup op stanMatrixOperators =
    name ++" = "++ a ++"("++ stanNodeRef i ++") "++ b ++" "++ c ++"("++ stanNodeRef j ++");"
  where (a,b,c) = fromJust $ lookup op stanMatrixOperators
stanNode name (Apply f js _) | isJust $ lookup f stanBuiltinFunctions =
    name ++" = "++ stanCompose fs (stanNodeRef `commas` js) ++";"
  where fs = fromJust $ lookup f stanBuiltinFunctions
stanNode name (Array sh (Lambda dag ret) _) =
    forLoop (map stanId $ inputs dag) sh $
        stanDAG Nothing dag ++"\n  "++
        name ++"["++ stanId  `commas` inputs dag ++"] = "++ stanNodeRef ret ++";"
stanNode name (FoldScan fs lr (Lambda dag ret) seed
               (Var ls s@(ArrayT _ ((Const 1 IntT,n):_) _)) t) =
    name ++ sloc ++" = "++ stanNodeRef seed ++";\n"++
    (forLoop [stanId idx] [(Const 1 IntT,n)] $ "  "++
      stanDecl False (stanId i) (typeIndex 1 s) ++"\n  "++
      stanDecl False (stanId j) (if fs == Fold then t else typeIndex 1 t) ++"\n  "++
      stanId i ++" = "++ stanId ls ++"["++ loc ++"];\n  "++
      stanId j ++" = "++ name ++ ploc ++";\n  "++
      "{\n"++ stanDAG Nothing dag ++"\n  "++
              name ++ rloc ++" = "++ stanNodeRef ret ++";\n  "++
      "}")
  where d = dagLevel dag
        idx = Dummy d 0
        [i,j] = inputs dag
        loc = case lr of
          Left_  -> stanId idx
          Right_ -> stanNodeRef n ++"+1-"++ stanId idx
        sloc = case fs of
          Fold -> ""
          Scan -> case lr of
            Left_ -> "[1]"
            Right_ -> "["++ stanNodeRef n ++"+1]"
        (ploc,rloc) = case fs of
          Fold -> ("","")
          Scan -> case lr of
            Left_  -> ("["++ loc ++"]", "["++ loc ++"+1]")
            Right_ -> ("["++ loc ++"+1]", "["++ loc ++"]")
stanNode _ node = error $ "unable to codegen node "++ show node

stanPNode :: Label -> PNode -> String
stanPNode name (Dist f args t) | isJust f' && length (typeDims t) > 1 =
  "to_vector("++ name ++") ~ "++ fromJust f' ++"("++ g `commas` args ++");"
  where f' = lookup f stanVectorisedDistributions
        g arg = "to_vector("++ stanNodeRef arg ++")"
stanPNode name d@(Dist f args t)
  | isJust $ lookup f stanBuiltinDistributions =
    name ++" ~ "++ fromJust (lookup f stanBuiltinDistributions) ++
      "("++ stanNodeRef `commas` args ++")"++ suffix ++";"
  | otherwise = error $ "stanPNode "++ show d
  where suffix | t == boolT = ""
               | (SubrangeT _ a b) <- t =
                   " T["++ maybe "" stanNodeRef a ++
                     ","++ maybe "" stanNodeRef b ++"]"
               | otherwise = ""
stanPNode name (Loop sh (Lambda ldag body) _) =
    forLoop (map stanId $ inputs ldag) sh $
        let lval = name ++"["++ stanId `commas` inputs ldag ++"]"
        in stanDAG Nothing ldag ++ indent (stanPNode lval body)
stanPNode name (HODist "orderedSample" d [n] _) =
    forLoop [stanId (Dummy 1 1)] [(Const 1 IntT,n)] $
        let lval = name ++"["++ stanId (Dummy 1 1) ++"]"
        in indent (stanPNode lval d)

stanDAG :: Maybe (Set Id) -> DAG -> String
stanDAG whitelist dag = indent $ extract decl ++ extract stanNode
  where level = dagLevel dag
        extract f = unlines . flip map (nodes dag) $ \(i,n) ->
          if isJust whitelist && Internal level i `Set.notMember` fromJust whitelist
          then "" else f (stanId $ Internal level i) n
        decl _ (Apply "getExternal" _ _) = ""
        decl name node = stanDecl False name $ typeNode node


------------------------------------------------------------------------------
-- WHOLE PROGRAMS                                                           --
------------------------------------------------------------------------------

stanProgram :: PBlock -> String
stanProgram pb@(PBlock block _ given _) =
    "data {\n"++ printRefs (\i n ->
        if getId i `elem` map getId' (Map.keys given)
        then stanDecl True (stanNodeRef i) (typePNode n) else "") ++"\n}\n"++
    "parameters {\n"++ printRefs (\i n ->
        if getId i `elem` map getId' (Map.keys given)
        then "" else stanDecl True (stanNodeRef i) (typePNode n)) ++"\n}\n"++
    "model {\n"++
      stanDAG (Just tparams) (topDAG block) ++"\n\n"++
      printRefs (\i n -> if Set.member (fromJust $ getId i) tparams
                  then stanPNode (stanNodeRef i) n
                  else "") ++"\n}\n"
  where printRefs f = indent . unlines . map g . Map.toAscList $ pnodes pb
          where g (i,n) = f (Var i (typePNode n)) n
        tparams = modelSkeleton pb

stanVector :: (Show a) => [a] -> LC.ByteString
stanVector xs = "c(" <> LC.intercalate "," (LC.pack . show <$> xs) <> ")"

stanConstVal :: ConstVal -> LC.ByteString
stanConstVal c | dimension c == 0 = LC.pack $ show c
stanConstVal c | dimension c == 1 = case c of
  Exact  a -> stanVector (A.elems a)
  Approx a -> stanVector (A.elems a)
stanConstVal c = "structure(" <> v <> ", .Dim = " <> stanVector hi <> ")"
  where (lo,hi) = bounds c
        is = rrange (lo,hi)
        v = case c of
          Exact  a -> stanVector $ (a!) <$> is
          Approx a -> stanVector $ (a!) <$> is

stanDump :: Env -> LC.ByteString
stanDump = LC.unlines . map f . Map.toList
  where f (LVar n,x) = LC.pack (stanId n) <> " <- " <> stanConstVal x

stanRead :: [[LC.ByteString]] -> [Env]
stanRead (header:rs) =
  [Map.fromList [(LVar k, arrayStrings v) | (Right k, v) <- parseRow r] | r <- rs]
  where header' = [(stanId' (LC.unpack x), map (fst . fromJust . LC.readInteger) xs)
                  | (x:xs) <- LC.split '.' <$> header]
        parseRow = groupSort . zipWith (\(a,b) c -> (a,(b,c))) header'
stanRead _ = error "CSV file contains no data"


------------------------------------------------------------------------------
-- CMDSTAN INTEGRATION                                                      --
------------------------------------------------------------------------------

data StanMethod
  = StanSample
    { numSamples :: Int
    , numWarmup :: Int
    , saveWarmup :: Bool
    , thin :: Int
    , adaptEngaged :: Bool
    , hmcEngine :: StanHMCEngine
    , hmcMetric :: StanHMCMetric
    , hmcStepSize :: Double
    , hmcStepSizeJitter :: Double
    }

instance Show StanMethod where
  show StanSample{..} = unwords
    ["method=sample"
    ,"num_samples="++ show numSamples
    ,"num_warmup="++ show numWarmup
    ,"save_warmup="++ if saveWarmup then "1" else "0"
    ,"thin="++ show thin
    ,"adapt engaged="++ if adaptEngaged then "1" else "0"
    ,"algorithm=hmc", show hmcEngine, show hmcMetric
    ,"stepsize="++ show hmcStepSize
    ,"stepsize_jitter="++ show hmcStepSizeJitter
    ]

data StanHMCEngine
  = StanStaticHMCEngine
    { intTime :: Double
    }
  | StanNUTSEngine
    { maxDepth :: Int
    }

instance Show StanHMCEngine where
  show StanStaticHMCEngine{..} = "engine=static int_time="++ show intTime
  show StanNUTSEngine{..} = "engine=nuts max_depth="++ show maxDepth

data StanHMCMetric
  = StanUnitEMetric
  | StanDiagEMetric
  | StanDenseEMetric

instance Show StanHMCMetric where
  show StanUnitEMetric  = "metric=unit_e"
  show StanDiagEMetric  = "metric=diag_e"
  show StanDenseEMetric = "metric=dense_e"

defaultStanMethod :: StanMethod
defaultStanMethod = StanSample
  { numSamples = 1000
  , numWarmup = 1000
  , saveWarmup = False
  , thin = 1
  , adaptEngaged = True
  , hmcEngine = StanNUTSEngine
    { maxDepth = 10 }
  , hmcMetric = StanDiagEMetric
  , hmcStepSize = 1.0
  , hmcStepSizeJitter = 0.0
  }

runStan :: (ExprTuple t) => StanMethod -> Prog t -> Maybe t -> IO [t]
runStan method prog init = withSystemTempDirectory "stan" $ \tmpDir -> do
    let basename = tmpDir ++"/stan"
    pwd <- getCurrentDirectory
    let hash = toHex . SHA1.hash . C.pack $ stanProgram p
        exename = pwd ++"/cache/stan/model_"++ hash
    exeExists <- doesFileExist exename

    unless exeExists $ do
      putStrLn' "--- Generating Stan code ---"
      putStrLn' $ stanProgram p
      writeFile (exename ++".stan") $ stanProgram p
      system_ $ "make -C "++ pwd ++"/cmdstan "++ exename

    when (isJust init) $
      LC.writeFile (basename ++".init") $
        stanDump $ unifyTuple block rets (fromJust init) given

    putStrLn' "--- Sampling Stan model ---"
    LC.writeFile (basename ++".data") . stanDump $ Map.map fst given
    let args = words (show method) ++
               ["data"
               ,"file="++ basename ++".data"
               ,"init="++ if isJust init then basename ++".init" else "0"
               ,"output"
               ,"file="++ basename ++".csv"
               ]
    putStrLn' $ unwords (exename:args)
    (_,_,_,pid) <- createProcess_ "stan" (proc exename args)--{std_out = UseHandle stderr}
    _ <- waitForProcess pid
    content <- LC.lines <$> LC.readFile (basename ++".csv")
    let table = map (LC.split ',') $ filter noComment content
    putStrLn' $ "Extracting: "++ stanNodeRef `commas` rets

    putStrLn' "--- Removing temporary files ---"
    return [fromRight' $ evalPBlock p rets env | env <- stanRead table]
  where (rets,p@(PBlock block _ given _)) =
          isolateNodeRefPBlock isBlockArray $ runProgExprs "stan" prog
        noComment row = not (LC.null row) && LC.head row /= '#'

-- TODO: deprecate these
hmcStan :: (ExprTuple t) => Int -> Prog t -> IO [t]
hmcStan n prog = runStan method prog Nothing
  where method = defaultStanMethod { numSamples = n, numWarmup = n }

hmcStanInit :: (ExprTuple t) => Int -> Prog t -> t -> IO [t]
hmcStanInit n prog t = runStan method prog (Just t)
  where method = defaultStanMethod { numSamples = n, numWarmup = n }
