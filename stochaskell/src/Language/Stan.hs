module Language.Stan where

import Control.Applicative ()
import Data.Array.Abstract
import Data.Expression
import Data.List
import Data.Maybe
import Data.Program
import Data.Ratio

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
    where f i (Interval a s) =
            "for ("++ stanId i ++" in "++ stanNodeRef a ++":"++
                 "("++ stanNodeRef a ++"+"++ stanNodeRef s ++"-1)) "


------------------------------------------------------------------------------
-- IDENTIFIERS                                                              --
------------------------------------------------------------------------------

stanId :: Id -> String
stanId (Id Builtin s) = s
stanId (Id (Dummy level) s) = "index_"++ show level ++"_"++ s
stanId (Id (Volatile level) s) = "sample_"++ show level ++"_"++ s

stanNodeRef :: NodeRef -> String
stanNodeRef (Internal level i) = "value_"++ show level ++"_"++ show i
stanNodeRef (External s _) = stanId s
stanNodeRef (Constant c) =
    if d == 1 then show n else show (fromRational c :: Double)
  where n = numerator c
        d = denominator c
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
      "["++ stanNodeRef `commas` map cardinality n ++"]"

stanDecl :: Label -> Type -> String
stanDecl name (ArrayT Nothing n t) =
    stanType t ++" "++ name ++
      "["++ stanNodeRef `commas` map cardinality n ++"];"
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
  ]

stanNode :: Label -> Node -> String
stanNode name (Apply op [i,j] _) | s /= "" =
    name ++" <- "++ stanNodeRef i ++" "++ s ++" "++ stanNodeRef j ++";"
  where s = fromMaybe "" $ lookup (stanId op) stanOperators
stanNode name (Apply f js _) =
    name ++" <- "++ fromMaybe s (lookup s stanBuiltinFunctions) ++
      "("++ stanNodeRef `commas` js ++");"
  where s = stanId f
stanNode name (Array _ sh dag ret _) =
    forLoop (inputs dag) sh $
        stanDAG dag ++"\n  "++
        name ++"["++ stanId  `commas` inputs dag ++"] <- "++ stanNodeRef ret ++";"

stanPNode :: Label -> PNode -> String
stanPNode name (Dist f args _) =
    name ++" ~ "++ fromMaybe s (lookup s stanBuiltinDistributions) ++
      "("++ stanNodeRef `commas` args ++");"
  where s = stanId f
stanPNode name (Loop sh body r _) =
    forLoop (inputs ldag) sh $
        let lval = name ++"["++ stanId `commas` inputs ldag ++"]"
        in stanPBlock lval r body
  where ldag = head $ definitions body


------------------------------------------------------------------------------
-- BLOCKS                                                                   --
------------------------------------------------------------------------------

stanDAG :: DAG -> String
stanDAG dag = indent $
    extract (\name -> stanDecl name . typeNode) ++
    extract stanNode
  where extract f = unlines . flip map (nodes dag) $ \(i,n) ->
                        let name = stanNodeRef $ Internal (dagLevel dag) i
                        in f name n

stanPBlock :: Label -> NodeRef -> PBlock -> String
stanPBlock rName rId (PBlock block refs _) =
    stanDAG (head block) ++ pdefs
  where pdefs = indent . unlines . filter (/= "") $
                    zipWith f [0..] (reverse refs)
        f i n = stanPNode (if cId == rId then rName else stanNodeRef cId) n
          where cId = External (Id (Volatile . dagLevel $ head block)
                                   (show i)) (typePNode n)
