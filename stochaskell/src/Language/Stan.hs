module Language.Stan where

import Control.Applicative ()
import Data.Array.Abstract
import Data.Expression
import Data.Expression.Const
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Program
import Data.Ratio
import GHC.Exts
import System.IO.Temp
import System.Process

-- TODO: better handling for exceptions to 1-based indexing

type Label = String

------------------------------------------------------------------------------
-- UTILITY FUNCTIONS                                                        --
------------------------------------------------------------------------------

commas :: (a -> String) -> [a] -> String
commas f xs = intercalate ", " $ map f xs

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

forLoop :: [Id] -> [Interval NodeRef] -> String -> String
forLoop is sh body = concat (zipWith f is sh) ++"{\n"++ body ++"\n}"
    where f i (a,b) =
            "for ("++ stanId i ++" in "++ stanNodeRef a
                                  ++":"++ stanNodeRef b ++") "


------------------------------------------------------------------------------
-- IDENTIFIERS                                                              --
------------------------------------------------------------------------------

stanId :: Id -> String
stanId (Dummy    level i) =  "index_"++ show level ++"_"++ show i
stanId (Volatile level i) = "sample_"++ show level ++"_"++ show i
stanId (Internal level i) =  "value_"++ show level ++"_"++ show i

stanConstVal :: ConstVal -> String
stanConstVal (Exact a) =
    if d == 1 then show n else show (fromRational c :: Double)
  where c = toScalar a
        n = numerator c
        d = denominator c

stanNodeRef :: NodeRef -> String
stanNodeRef (Var s _) = stanId s
stanNodeRef (Const c) = stanConstVal c
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
  [("asVector",    "to_vector")
  ,("asMatrix",    "to_matrix")
  ,("chol",        "cholesky_decompose")
  ,("ifThenElse",  "if_else")
  ]

stanBuiltinDistributions =
  [("bernoulliLogit",  "bernoulli_logit")
  ]

stanOperators =
  [("+",   "+")
  ,("-",   "-")
  ,("*",   "*")
  ,("/",   "/")
  ,("==",  "==")
  ,("#>",  "*")
  ,("**",  "^")
  ]

stanNode :: Label -> Node -> String
stanNode name (Apply op [i,j] _) | s /= "" =
    name ++" <- "++ stanNodeRef i ++" "++ s ++" "++ stanNodeRef j ++";"
  where s = fromMaybe "" $ lookup op stanOperators
stanNode name (Apply f js _) =
    name ++" <- "++ fromMaybe f (lookup f stanBuiltinFunctions) ++
      "("++ stanNodeRef `commas` js ++");"
stanNode name (Array sh dag ret _) =
    forLoop (inputs dag) sh $
        stanDAG dag ++"\n  "++
        name ++"["++ stanId  `commas` inputs dag ++"] <- "++ stanNodeRef ret ++";"

stanPNode :: Label -> PNode -> String
stanPNode name (Dist f args _) =
    name ++" ~ "++ fromMaybe f (lookup f stanBuiltinDistributions) ++
      "("++ stanNodeRef `commas` args ++");"
stanPNode name (Loop sh ldag body _) =
    forLoop (inputs ldag) sh $
        let lval = name ++"["++ stanId `commas` inputs ldag ++"]"
        in stanDAG ldag ++ indent (stanPNode lval body)

stanDAG :: DAG -> String
stanDAG dag = indent $
    extract (\name -> stanDecl name . typeNode) ++
    extract stanNode
  where extract f = unlines . flip map (nodes dag) $ \(i,n) ->
                        let name = stanId $ Internal (dagLevel dag) i
                        in f name n


------------------------------------------------------------------------------
-- WHOLE PROGRAMS                                                           --
------------------------------------------------------------------------------

stanProgram :: PBlock -> String
stanProgram (PBlock block refs given) =
    "data {\n"++ printRefs (\i n ->
        if i `elem` map fst given
        then stanDecl (stanNodeRef i) (typePNode n) else "") ++"\n}\n"++
    "parameters {\n"++ printRefs (\i n ->
        if i `elem` map fst given
        then "" else stanDecl (stanNodeRef i) (typePNode n)) ++"\n}\n"++
    "transformed parameters {\n"++ stanDAG (head block) ++"\n}\n"++
    "model {\n"++ printRefs (\i n -> stanPNode (stanNodeRef i) n) ++"\n}\n"
  where printRefs f = indent . unlines $ zipWith g [0..] (reverse refs)
          where g i n = f (Var (Volatile 0 i) (typePNode n)) n

runStan :: ProgE [Double] -> IO [[Double]]
runStan prog = withSystemTempDirectory "stan" $ \tmpDir -> do
    let basename = tmpDir ++"/generated_model"

    putStrLn "--- Generating Stan code ---"
    putStrLn $ stanProgram p
    writeFile (basename ++".stan") $ stanProgram p
    system $ "stan "++ basename

    putStrLn "--- Sampling Stan model ---"
    let dat = unlines . flip map (constraints p) $ \(n,ar) ->
                stanNodeRef n ++" <- c("++
                  (stanNodeRef . Const . fromRational) `commas` toList ar ++")"
    putStrLn dat
    writeFile (basename ++".data") dat
    system $ basename ++" sample data file="++ basename ++".data "++
                              "output file="++ basename ++".csv"
    content <- readFile $ basename ++".csv"

    putStrLn "--- Removing temporary files ---"
    return $ map (map read) (tail $ extractCSV content)
  where ((r,_),p) = runProgE prog
        extractCSV content = map (take (length cols) . drop (head cols)) table
          where noComment row = row /= "" && head row /= '#'
                table = map (splitOn ",") . filter noComment $ lines content
                cols = findIndices (isPrefixOf $ stanNodeRef r ++ ".") (head table)