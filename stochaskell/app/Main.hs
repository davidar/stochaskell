{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Expression
import Data.List
import Data.Program
import Data.Ratio

forLoop is sh = concat . map (\s -> "for ("++ s ++") ") $ zipWith g is sh
    where g i (Interval a s) = stan i ++" in "++ stan a ++":"++ upper
            where upper = if stan a == "1"
                            then stan s
                            else "("++ stan a ++"+"++ stan s ++"-1)"

class Stan a where
    stan :: a -> String

instance (Stan a) => Stan [a] where
    stan xs = "["++ intercalate ", " (map stan xs) ++"]"

instance Stan Scope where
    stan Builtin = ""
    stan (Dummy level) = "index_"++ show level ++"_"
    stan (Volatile level) = "sample_"++ show level ++"_"

instance Stan Id where
    stan (Id scope s) = stan scope ++ s

stanNode name (Apply op [i,j] t) | op `elem` ["+","-","*","/","=="] =
    name ++" <- "++ stan i ++" "++ stan op ++" "++ stan j ++";"
stanNode name (Apply "#>" [i,j] t) =
    name ++" <- "++ stan i ++" * "++ stan j ++";"
stanNode name (Apply f js t) =
    name ++" <- "++ g ++"("++ intercalate ", " (map stan js) ++");"
  where g = case lookup (stan f) funcs of
                Just g  -> g
                Nothing -> stan f
        funcs = [("asVector","to_vector")
                ,("asMatrix","to_matrix")
                ,("chol","cholesky_decompose")
                ,("ifThenElse","if_else")
                ]
stanNode name (Array _ sh dag@(DAG level is _) ret t) =
    forLoop is sh ++"{\n"++ body ++"\n}"
  where body = stan dag ++ (if stan dag == "" then "    " else "\n    ") ++
                name ++ stan is ++" <- "++ stan ret ++";"

instance Stan NodeRef where
    stan (Internal level i) = "value_"++ show level ++"_"++ show i
    stan (External s _) = stan s
    stan (Constant c) = if d == 1 then show n
                                  else show (fromRational c :: Double)
      where n = numerator c
            d = denominator c
    stan (Index f js) = stan f ++ stan (map g $ reverse js)
      where g j = j

instance Stan DAG where
    stan (DAG level _ bmap) = intercalate "\n" . map ("    "++) $
        lines (decls ++ if decls == "" then ""  else "\n"++ defs)
      where defs  = unlines . map f $ Bimap.toAscListR bmap
            decls = unlines . map g $ Bimap.toAscListR bmap
            f (i,n) = stanNode (stan $ Internal level i) n
            g (i,n) = stanDecl (typeNode n) (stan $ Internal level i)

instance Stan Type where
    stan IntT = "int"
    stan RealT = "real"
    stan (SubrangeT t _ _) = stan t
    {- TODO:
    stan (SubrangeT t Nothing  Nothing ) = stan t
    stan (SubrangeT t (Just l) Nothing ) = stan t ++"<lower="++ stan l ++">"
    stan (SubrangeT t Nothing  (Just h)) = stan t ++"<upper="++ stan h ++">"
    stan (SubrangeT t (Just l) (Just h)) = stan t ++"<lower="++ stan l ++
                                                    ",upper="++ stan h ++">"
    -}
    stan (ArrayT Nothing n t) =   stan t ++ stan (map cardinality n)
    stan (ArrayT (Just kind) n _) = kind ++ stan (map cardinality n)

stanDecl (ArrayT Nothing n t) name =
    stan t ++" "++ name ++ stan (map cardinality n) ++";"
stanDecl t name = stan t ++" "++ name ++";"

stanPBlock rName rId (PBlock block revrefs given) =
    if depth == 0
       then "data {\n"++ dat' ++"\n}\n\n"++
            "parameters {\n"++ params' ++"\n}\n\n"++
            "transformed parameters {\n"++ edefs ++"\n}\n\n"++
            "model {\n"++ pdefs' ++"\n}\n"++
            "/*\ncollect: "++ rName ++" = "++ stan rId ++"\n"++
                "constraints:\n"++ constrs ++"*/\n"
       else edefs ++ (if edefs == "" then "" else "\n") ++ pdefs'
  where refs = reverse revrefs
        depth = dagLevel $ head block
        pdefs = zipWith f [0..] refs
          where f i n = stanPNode name n
                  where ident = External (Id (Volatile depth) (show i)) (typePNode n)
                        name = if ident == rId then rName else stan ident
        params = zipWith g [0..] refs
          where g i n = if ident `elem` map fst given
                            then ""
                            else stanDecl (typePNode n) name
                  where ident = External (Id (Volatile depth) (show i)) (typePNode n)
                        name = if ident == rId then rName else stan ident
        dat = zipWith g [0..] refs
          where g i n = if ident `elem` map fst given
                            then stanDecl (typePNode n) name
                            else ""
                  where ident = External (Id (Volatile depth) (show i)) (typePNode n)
                        name = if ident == rId then rName else stan ident
        edefs = stan (head block)
        indent = intercalate "\n" . map ("    "++) . lines
        pdefs'  = indent . unlines $ pdefs
        params' = indent . unlines $ filter (/="") params
        dat' = indent . unlines $ filter (/="") dat
        constrs = unlines $ map f given
          where f (n,ar) = stan n ++" <- c("++ intercalate ", " (map g ar) ++")"
                g c = let x = fromRational c :: Double in show x

stanPNode name (Dist f args t) =
    name ++" ~ "++ g ++"("++ intercalate ", " (map stan args) ++");"
  where g = case lookup (stan f) funcs of
                Just g  -> g
                Nothing -> stan f
        funcs = [("bernoulliLogit","bernoulli_logit")]
stanPNode name (Loop sh body r t) =
    forLoop (inputs ldag) sh ++
    "{\n"++ stanPBlock (name ++ stan (inputs ldag)) r body ++"\n}"
  where (PBlock (ldag:_) _ _) = body

instance Stan (Prog (Expr t)) where
    stan p = stanPBlock "RESULT" r pblock
      where ((r,_), pblock) = runState (fromProgE p) emptyPBlock

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

posterior = do
    (g,phi,s) <- prior 100
    assume phi $ map (\i -> if odd i then 1 else 0) [1..100]
    assume s   $ map (\i -> if odd i then 1 else 0) [1..100]
    return g

main = putStrLn $ stan posterior
