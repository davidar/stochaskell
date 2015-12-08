{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables #-}
module Data.Program where

import Control.Applicative
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Expression (R,ExprType,Expr,DExpr,fromExpr,fromDExpr,typeDExpr)
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
                  , typePNode :: Expr.Type
                  }
           | Loop { lShape :: [Interval Expr.NodeRef]
                  , lBody  :: PBlock
                  , lRet   :: Expr.NodeRef
                  , typePNode :: Expr.Type
                  }
           deriving (Eq)

data PBlock = PBlock { definitions :: Expr.Block
                     , actions     :: [PNode]
                     , constraints :: [(Expr.NodeRef, [Rational])]
                     }
            deriving (Eq)
emptyPBlock = PBlock Expr.emptyBlock [] []

-- lift into Expr.Block
liftExprBlock state = do
    PBlock block rhs given <- get
    let (ret, block') = runState state block
    put $ PBlock block' rhs given
    return ret

data Prog t = Prog { fromProg :: State PBlock t }
instance (Eq t) => Eq (Prog t) where p == q = runProg p == runProg q
runProg p = runState (fromProg p) emptyPBlock

fromProgE :: Prog (Expr t) -> State PBlock (Expr.NodeRef, Expr.Type)
fromProgE p = do
    e <- fromProg p
    j <- liftExprBlock $ fromExpr e
    return (j, typeDExpr $ Expr.erase e)

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
-- PRIMITIVE DISTRIBUTIONS                                                  --
------------------------------------------------------------------------------

dist s = Prog $ do
    d <- liftExprBlock s
    PBlock block rhs given <- get
    put $ PBlock block (d:rhs) given
    let depth = Expr.dagLevel $ head block
        k = length rhs
        name = Expr.Id (Expr.Volatile depth) (show k)
        v = Expr.expr . return $ Expr.External name (typePNode d)
    return v

normal :: Expr R -> Expr R -> Prog (Expr R)
normal m s = dist $ do
    i <- fromExpr m
    j <- fromExpr s
    return $ Dist (Expr.Id Expr.Builtin "normal") [i,j] Expr.RealT

bernoulliLogit :: Expr R -> Prog (Expr Bool)
bernoulliLogit l = dist $ do
    i <- fromExpr l
    return $ Dist (Expr.Id Expr.Builtin "bernoulliLogit") [i] t
  where (Expr.TypeIs t) = Expr.typeOf :: Expr.TypeOf Bool


------------------------------------------------------------------------------
-- LOOPS                                                                    --
------------------------------------------------------------------------------

makeLoop shape body = do
    (r,t) <- fromProgE body
    sh <- liftExprBlock . Expr.liftBlock $ sequence shape'
    block <- get
    return $ Loop sh block r (Expr.ArrayT Nothing sh t)
  where shape' = flip map shape $ \interval -> do
            i <- fromExpr $ lowerBound  interval
            z <- fromExpr $ cardinality interval
            return $ Interval i z

joint :: forall r f. (ExprType r, ExprType f)
      => (AbstractArray (Expr Integer)       (Expr r)  ->       Expr f)
      ->  AbstractArray (Expr Integer) (Prog (Expr r)) -> Prog (Expr f)
joint _ ar = Prog $ do
    PBlock block dists given <- get
    let ashape = shape ar
        depth = Expr.dagLevel $ head block
        ids = [ Expr.Id (Expr.Dummy $ depth + 1) (show i) | i <- [1..length ashape] ]
        p = apply ar [Expr.expr . return $ Expr.External i Expr.IntT | i <- ids]
        loop = evalState (makeLoop ashape p) $
            PBlock (Expr.DAG (depth + 1) ids Bimap.empty : block) [] []
        name = Expr.Id (Expr.Volatile depth) $ show (length dists)
        ref = Expr.expr . return $ Expr.External name t :: Expr f
        t = typePNode loop
    put $ let (PBlock (_:block') _ _) = lBody loop
           in PBlock block' (loop:dists) given
    return ref


------------------------------------------------------------------------------
-- CONDITIONING                                                             --
------------------------------------------------------------------------------

assume :: Expr t -> [Rational] -> Prog ()
assume cond dat = Prog $ do
    i <- liftExprBlock (fromExpr cond)
    (PBlock block dists given) <- get
    put $ PBlock block dists ((i,dat):given)
