{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Stan where

import Control.Applicative ()
import Control.Monad
import qualified Data.Array.IArray as A
import Data.Array.Abstract hiding ((<>))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Expression hiding (const)
import Data.Expression.Const
import Data.Expression.Eval
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Program
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import qualified Crypto.Hash.SHA1 as SHA1
import GHC.Exts
import System.Directory
import System.IO.Temp
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
stanId (Dummy    level i) =  "index_"++ show level ++"_"++ show i
stanId (Volatile level i) = "sample_"++ show level ++"_"++ show i
stanId (Internal level i) =  "value_"++ show level ++"_"++ show i

stanNodeRef :: NodeRef -> String
stanNodeRef (Var s _) = stanId s
stanNodeRef (Const c _) = show c
stanNodeRef (Index f js) =
    stanNodeRef f ++"["++ stanNodeRef `commas` reverse js ++"]"


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

stanType :: Type -> String
stanType IntT = "int"
stanType RealT = "real"
stanType (SubrangeT t _ _) = stanType t
{- TODO only enable at top-level:
stanType (SubrangeT t Nothing  Nothing ) = stan t
stanType (SubrangeT t (Just l) Nothing ) = stan t ++"<lower="++ stan l ++">"
stanType (SubrangeT t Nothing  (Just h)) = stan t ++"<upper="++ stan h ++">"
stanType (SubrangeT t (Just l) (Just h)) = stan t ++"<lower="++ stan l ++
                                                    ",upper="++ stan h ++">"
-}
stanType (ArrayT kind n t) =
    fromMaybe (stanType t) kind ++
      "["++ stanNodeRef `commas` map snd n ++"]"

stanDecl :: Label -> Type -> String
stanDecl name (ArrayT Nothing n t) =
    stanType t ++" "++ name ++
      "["++ stanNodeRef `commas` map snd n ++"];"
stanDecl name t = stanType t ++" "++ name ++";"


------------------------------------------------------------------------------
-- NODES                                                                    --
------------------------------------------------------------------------------

stanBuiltinFunctions =
  [("chol",        ["cholesky_decompose", "to_matrix"])
  ,("det",         ["determinant",        "to_matrix"])
  ,("inv",         ["inverse",            "to_matrix"])
  ,("negate",      ["-"])
  ]

stanVectorisedDistributions =
  [("bernoulliLogits", "bernoulli_logit")
  ,("normals",         "normal")
  ,("uniforms",        "uniform")
  ]

stanBuiltinDistributions =
  [("bernoulliLogit",  "bernoulli_logit")
  ] ++ stanVectorisedDistributions

stanOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   "*")
  ,("*>",  "*")
  ,("/",   "/")
  ,("==",  "==")
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
stanNode name (Apply "ifThenElse" [a,b,c] _) =
    name ++" = "++ stanNodeRef a ++" ? "++ stanNodeRef b ++" : "++ stanNodeRef c ++";"
stanNode name (Apply "tr'" [a] _) =
    name ++" = "++ stanNodeRef a ++"';"
stanNode name (Apply op [i,j] _) | s /= "" =
    name ++" = "++ stanNodeRef i ++" "++ s ++" "++ stanNodeRef j ++";"
  where s = fromMaybe "" $ lookup op stanOperators
stanNode name (Apply op [i,j] _) | isJust $ lookup op stanMatrixOperators =
    name ++" = "++ a ++"("++ stanNodeRef i ++") "++ b ++" "++ c ++"("++ stanNodeRef j ++");"
  where (a,b,c) = fromJust $ lookup op stanMatrixOperators
stanNode name (Apply f js _) | isJust $ lookup f stanBuiltinFunctions =
    name ++" = "++ stanCompose fs (stanNodeRef `commas` js) ++";"
  where fs = fromJust $ lookup f stanBuiltinFunctions
stanNode name (Array sh dag ret _) =
    forLoop (map stanId $ inputs dag) sh $
        stanDAG Nothing dag ++"\n  "++
        name ++"["++ stanId  `commas` inputs dag ++"] = "++ stanNodeRef ret ++";"
stanNode name (Fold Right_ dag ret seed (Var ls at@(ArrayT _ ((lo,hi):_) _)) t) =
    name ++" = "++ stanNodeRef seed ++";\n"++ loop
  where d = dagLevel dag
        idx = Dummy d 0
        [i,j] = inputs dag
        s = typeIndex at
        loop =
          forLoop [stanId idx] [(lo,hi)] $ "  "++
           stanDecl (stanId i) s ++"\n  "++
           stanDecl (stanId j) t ++"\n  "++
           stanId i ++" = "++ stanId ls ++"["++
             stanNodeRef lo ++"+"++ stanNodeRef hi ++"-"++ stanId idx ++"];\n  "++
           stanId j ++" = "++ name ++";\n  {\n"++
           stanDAG Nothing dag ++"\n  "++
           name ++" = "++ stanNodeRef ret ++";\n  }"
stanNode _ node = error $ "unable to codegen node "++ show node

stanPNode :: Label -> PNode -> String
stanPNode name (Dist f args t) | isJust f' && length dims > 1 =
  forLoop idxs dims $
    "  "++ name ++ idxs' ++" ~ "++ fromJust f' ++"("++ g `commas` args ++");"
  where f' = lookup f stanVectorisedDistributions
        dims = typeDims t
        idxs = [name ++"_index_"++ show i | i <- [1..length dims]]
        idxs' = "["++ intercalate "," idxs ++"]"
        g arg = stanNodeRef arg ++ idxs'
stanPNode name (Dist f args _) =
    name ++" ~ "++ fromMaybe f (lookup f stanBuiltinDistributions) ++
      "("++ stanNodeRef `commas` args ++");"
stanPNode name (Loop sh ldag body _) =
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
        decl name node = stanDecl name $ typeNode node


------------------------------------------------------------------------------
-- WHOLE PROGRAMS                                                           --
------------------------------------------------------------------------------

stanProgram :: PBlock -> String
stanProgram (PBlock block refs given) =
    "data {\n"++ printRefs (\i n ->
        if getId i `elem` map Just (Map.keys given)
        then stanDecl (stanNodeRef i) (typePNode n) else "") ++"\n}\n"++
    "parameters {\n"++ printRefs (\i n ->
        if getId i `elem` map Just (Map.keys given)
        then "" else stanDecl (stanNodeRef i) (typePNode n)) ++"\n}\n"++
    "model {\n"++
      stanDAG (Just tparams) (head block) ++"\n\n"++
      printRefs (\i n -> if Set.member (fromJust $ getId i) tparams
                  then stanPNode (stanNodeRef i) n
                  else "") ++"\n}\n"
  where printRefs f = indent . unlines . map g $ Map.toAscList samples
          where g (i,n) = f (Var i (typePNode n)) n
        dependsPNode (Dist _ args _) =
          Set.unions $ map (dependsNodeRef block) args
        samples = Map.fromList $ map (Volatile 0) [0..] `zip` reverse refs
        params = Map.keysSet samples Set.\\ Map.keysSet given
        dependents xs = Set.union xs . Map.keysSet $
          Map.filter (not . Set.null . Set.intersection xs . dependsPNode) samples
        dparams = fixpt dependents params
        tparams = Set.foldr Set.union dparams $ Set.map g dparams
        -- TODO: warn when any samples\\tparams have zero density
          where g i = let n = fromJust $ Map.lookup i samples
                      in Set.filter isInternal $ dependsPNode n

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
  where f (n,x) = LC.pack (stanId n) <> " <- " <> stanConstVal x


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
      putStrLn "--- Generating Stan code ---"
      putStrLn $ stanProgram p
      writeFile (exename ++".stan") $ stanProgram p
      system_ $ "make -C "++ pwd ++"/cmdstan "++ exename

    when (isJust init) $
      LC.writeFile (basename ++".init") $
        stanDump (unifyTuple' (definitions p) rets (fromJust init) (constraints p))

    putStrLn "--- Sampling Stan model ---"
    LC.writeFile (basename ++".data") $ stanDump (constraints p)
    -- TODO: avoid shell injection
    system_ $ unwords
      [exename
      ,show method
      ,"data file="++ basename ++".data"
      ,"init="++ if isJust init then basename ++".init" else "0"
      ,"output file="++ basename ++".csv"
      ]
    content <- lines <$> readFile (basename ++".csv")
    putStrLn . unlines $ filter (('#'==) . head) content
    putStrLn $ "Extracting: "++ stanNodeRef `commas` rets

    putStrLn "--- Removing temporary files ---"
    -- TODO: will fail if any rets are deterministic transforms of parameters
    return $ extractCSV content
  where (rets,p) = runProgExprs prog
        extractCSV content = map f (tail table)
          where noComment row = row /= "" && head row /= '#'
                table = map (splitOn ",") $ filter noComment content
                readDouble = read :: String -> Double
                slice xs = map (xs !!)
                f row = fromConstVals $ do
                  r <- rets
                  return $ case stanNodeRef r `elemIndex` head table of
                    Just col -> real . readDouble $ row !! col
                    Nothing ->
                      let prefix = stanNodeRef r ++ "."
                          cols = isPrefixOf prefix `findIndices` head table
                      in fromList (real . readDouble <$> row `slice` cols)

-- TODO: deprecate these
hmcStan :: (ExprTuple t) => Int -> Prog t -> IO [t]
hmcStan n prog = runStan method prog Nothing
  where method = defaultStanMethod { numSamples = n, numWarmup = n }

hmcStanInit :: (ExprTuple t) => Int -> Prog t -> t -> IO [t]
hmcStanInit n prog t = runStan method prog (Just t)
  where method = defaultStanMethod { numSamples = n, numWarmup = n }
