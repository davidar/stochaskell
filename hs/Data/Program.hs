{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables #-}
module Data.Program where

import Control.Applicative
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Expression (R,ExprType,Expr,DExpr,fromExpr,fromDExpr,exprType)
import qualified Data.Expression as Expr
import Data.List
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad.State


------------------------------------------------------------------------------
-- PROGRAMS                                                                 --
------------------------------------------------------------------------------

data PNode = Dist { dName :: Expr.Id
                  , dArgs :: [Expr.NodeRef]
                  , dType :: Expr.Type
                  }
           | Loop { lShape :: [Expr.NodeRef]
                  , lBody  :: PBlock
                  , lRet   :: Expr.NodeRef
                  , lRetTy :: Expr.Type
                  }
           deriving (Eq)

data PBlock = PBlock Expr.Block [PNode]
            deriving (Eq)
emptyPBlock = PBlock Expr.emptyBlock []

-- lift into Expr.Block
liftExprBlock state = do
    PBlock block rhs <- get
    let (ret, block') = runState state block
    put $ PBlock block' rhs
    return ret

data Prog t = Prog  { fromProg  :: State PBlock t }
instance (Eq t) => Eq (Prog t) where p == q = runProg p == runProg q
runProg p = runState (fromProg p) emptyPBlock

fromProgE :: Prog (Expr t) -> State PBlock (Expr.NodeRef, Expr.Type)
fromProgE p = do
    e <- fromProg p
    j <- liftExprBlock $ fromExpr e
    return (j, exprType $ Expr.erase e)

instance Functor Prog where
    fmap = liftM
instance Applicative Prog where
    pure  = return
    (<*>) = ap
instance Monad Prog where
    return = Prog . return
    act >>= k = Prog $ do
        x <- fromProg act
        fromProg (k x)


------------------------------------------------------------------------------
-- DEBUGGING OUTPUT                                                         --
------------------------------------------------------------------------------

instance Show PBlock where
    show (PBlock block revrefs) =
        edefs ++ (if edefs == "" then "" else "\n")
              ++ (indent . unlines $ pdefs)
      where refs = reverse revrefs
            depth = Expr.dagLevel $ head block
            pdefs = zipWith f [0..] refs
              where f i n = "~" ++ show depth ++ "." ++
                                   show i ++ " <- " ++ show n
            edefs = show (head block)
            indent = intercalate "\n" . map ("    " ++) . lines

instance Show PNode where
    show (Dist f args t) =
        f ++ " " ++ intercalate " " (map show args) ++ " :: " ++ show t
    show (Loop sh body r t) =
        "(" ++ intercalate ", " (zipWith g [0..] sh) ++ ") " ++
        "[ " ++ "do\n" ++ show body ++ "\n" ++
        "    return "  ++ show r ++ " :: " ++ show t ++ " ]"
      where g i b = Expr.inputs ldag !! i ++ " < " ++ show b
            (PBlock (ldag:_) _) = body

instance Show (Prog (Expr t)) where
    show p = "main = " ++ ret ++ " where\n" ++ show pblock
      where ((r,t), pblock) = runState (fromProgE p) emptyPBlock
            (PBlock block _) = pblock
            ret = show r ++ " :: " ++ show t


------------------------------------------------------------------------------
-- PRIMITIVE DISTRIBUTIONS                                                  --
------------------------------------------------------------------------------

dist s = Prog $ do
    d <- liftExprBlock s
    PBlock block rhs <- get
    put $ PBlock block (d:rhs)
    let depth = Expr.dagLevel $ head block
        k = length rhs
    return . fromString $ "~" ++ show depth ++ "." ++ show k

normal :: Expr R -> Expr R -> Prog (Expr R)
normal m s = dist $ do
    i <- fromExpr m
    j <- fromExpr s
    return $ Dist "normal" [i,j] Expr.RealT

bernoulliLogit :: Expr R -> Prog (Expr Bool)
bernoulliLogit l = dist $ do
    i <- fromExpr l
    return $ Dist "bernoulliLogit" [i] t
  where (Expr.TypeIs t) = Expr.typeOf :: Expr.TypeOf Bool


------------------------------------------------------------------------------
-- LOOPS                                                                    --
------------------------------------------------------------------------------

makeLoop shape body = do
    (r,t) <- fromProgE body
    sh <- liftExprBlock . Expr.liftBlock . sequence $ map fromExpr shape
    block <- get
    return $ Loop sh block r t

joint :: forall r f. (ExprType r, ExprType f)
      => (AbstractArray (Expr Integer)       (Expr r)  ->       Expr f)
      ->  AbstractArray (Expr Integer) (Prog (Expr r)) -> Prog (Expr f)
joint transf ar = Prog $ do
    PBlock block dists <- get
    let ashape = shape ar
        depth = Expr.dagLevel $ head block
        ids = [ "%" ++ show (depth+1) ++ "." ++ show i
              | i <- [1..length ashape] ]
        p = apply ar [fromString i | i <- ids]
        loop = evalState (makeLoop ashape p) $
            PBlock (Expr.DAG (depth + 1) ids Bimap.empty : block) []
        name = "~" ++ show depth ++ "." ++ show (length dists)
    put $ let (PBlock (_:block') _) = lBody loop
           in PBlock block' (loop:dists)
    return . transf . AArr ashape $ Expr.index (fromString name :: Expr f)
