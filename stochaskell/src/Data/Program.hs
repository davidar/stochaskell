{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables,
             FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
module Data.Program where

import Control.Monad.Guard
import Control.Monad.State
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Expression (ExprType,Expr,fromExpr,typeDExpr)
import qualified Data.Expression as Expr
import Data.Expression.Const
import Data.Expression.Eval
import Data.Ix
import Data.Maybe
import qualified Data.Number.LogFloat as LF
import Data.Random.Distribution (logPdf)
import Data.Random.Distribution.Abstract
import Data.Random.Distribution.Normal (Normal(..))
import GHC.Exts


------------------------------------------------------------------------------
-- PROGRAMS                                                                 --
------------------------------------------------------------------------------

data PNode = Dist { dName :: String
                  , dArgs :: [Expr.NodeRef]
                  , typePNode :: Expr.Type
                  }
           | Loop { lShape :: [Interval Expr.NodeRef]
                  , lDefs  :: Expr.DAG
                  , lBody  :: PNode
                  , typePNode :: Expr.Type
                  }
           deriving (Eq)

data PBlock = PBlock { definitions :: Expr.Block
                     , actions     :: [PNode]
                     , constraints :: [(Expr.NodeRef, ConstVal)]
                     }
            deriving (Eq)
emptyPBlock :: PBlock
emptyPBlock = PBlock Expr.emptyBlock [] []

-- lift into Expr.Block
liftExprBlock :: MonadState PBlock m => State Expr.Block b -> m b
liftExprBlock s = do
    PBlock block rhs given <- get
    let (ret, block') = runState s block
    put $ PBlock block' rhs given
    return ret

data Prog t = Prog { fromProg :: State PBlock t }
instance (Eq t) => Eq (Prog t) where p == q = runProg p == runProg q
runProg :: Prog a -> (a, PBlock)
runProg p = runState (fromProg p) emptyPBlock

type ProgE t = Prog (Expr t)
fromProgE :: ProgE t -> State PBlock (Expr.NodeRef, Expr.Type)
fromProgE p = do
    e <- fromProg p
    j <- liftExprBlock $ fromExpr e
    return (j, typeDExpr $ Expr.erase e)
runProgE :: ProgE t -> ((Expr.NodeRef, Expr.Type), PBlock)
runProgE p = runState (fromProgE p) emptyPBlock

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

dist :: ExprType t => State Expr.Block PNode -> ProgE t
dist s = Prog $ do
    d <- liftExprBlock s
    PBlock block rhs given <- get
    put $ PBlock block (d:rhs) given
    let depth = Expr.dagLevel $ head block
        k = length rhs
        name = Expr.Volatile depth k
        v = Expr.expr . return $ Expr.Var name (typePNode d)
    return v

instance Distribution Bernoulli (Expr Double) Prog (Expr Bool) where
    sample (Bernoulli p) = dist $ do
        i <- fromExpr p
        return $ Dist "bernoulli" [i] t
      where (Expr.TypeIs t) = Expr.typeOf :: Expr.TypeOf Bool
    sample (BernoulliLogit l) = dist $ do
        i <- fromExpr l
        return $ Dist "bernoulliLogit" [i] t
      where (Expr.TypeIs t) = Expr.typeOf :: Expr.TypeOf Bool

instance Distribution Normal (Expr Double) Prog (Expr Double) where
    sample (Normal m s) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normal" [i,j] Expr.RealT
    sample StdNormal = sample (Normal 0 1)


------------------------------------------------------------------------------
-- LOOPS                                                                    --
------------------------------------------------------------------------------

instance forall r f. (ExprType r, ExprType f)
    => Joint Prog (Expr Integer) (Expr r) (Expr f) where
  joint _ ar = Prog $ do
    sh <- liftExprBlock . sequence . flip map (shape ar) $ \(a,b) -> do
      i <- fromExpr a
      j <- fromExpr b
      return (i,j)
    PBlock block dists given <- get
    let ids = [ Expr.Dummy (length block) i | i <- [1..length sh] ]
        p = ar ! [Expr.expr . return $ Expr.Var i Expr.IntT | i <- ids]
        ((_,t), PBlock (dag:block') [act] []) = runState (fromProgE p) $
            PBlock (Expr.DAG (length block) ids Bimap.empty : block) [] []
        loopType = Expr.ArrayT Nothing sh t
        loop = Loop sh dag act loopType
    put $ PBlock block' (loop:dists) given
    let name = Expr.Volatile (length block - 1) (length dists)
    return (Expr.expr . return $ Expr.Var name loopType :: Expr f)


------------------------------------------------------------------------------
-- CONDITIONING                                                             --
------------------------------------------------------------------------------

type instance ConditionOf (Prog ()) = Expr Bool
instance MonadGuard Prog where
    guard cond = Prog $ do -- TODO: weaker assumptions
        (Expr.Var (Expr.Internal 0 i) _) <- liftExprBlock (fromExpr cond)
        (PBlock (dag:dags) dists given) <- get
        if i /= length (Expr.nodes dag) - 1 then undefined else do
          let (Just (Expr.Apply "==" [j, Expr.Const a] _)) =
                lookup i $ Expr.nodes dag
              dag' = dag { Expr.bimap = Bimap.deleteR i (Expr.bimap dag) }
          put $ PBlock (dag':dags) dists ((j,a):given)


------------------------------------------------------------------------------
-- PROBABILITY DENSITIES                                                    --
------------------------------------------------------------------------------

density :: (Expr.ExprTuple t) => Prog t -> t -> LF.LogFloat
density prog vals = flip densityPBlock pb $ do
    (d,e) <- Expr.unify rets vals
    let (Expr.Var i _, _) = Expr.runDExpr d
        v = evalD [] e
    return (i,v)
  where (rets, pb) = runProg prog

densityPBlock :: Env -> PBlock -> LF.LogFloat
densityPBlock env (PBlock block refs _) = LF.product $ do
    (i,d) <- zip [0..] $ reverse refs
    let ident = Expr.Volatile (Expr.dagLevel $ head block) i
        val = flip fromMaybe (lookup ident env) $
          error $ "parameter "++ show ident ++
                  " not specified in env "++ show (map fst env)
    return $ densityPNode env block d val

densityPNode :: Env -> Expr.Block -> PNode -> ConstVal -> LF.LogFloat
densityPNode env block (Dist "normal" [m,s] _) x =
    LF.logToLogFloat $ logPdf (Normal m' s') (toDouble x)
  where m' = toDouble $ evalNodeRef env block m
        s' = toDouble $ evalNodeRef env block s
densityPNode env block (Dist "bernoulliLogit" [l] _) a
    | x == 1 = LF.logFloat p
    | x == 0 = LF.logFloat (1 - p)
    | otherwise = LF.logFloat 0
  where x = toRational a
        l' = toDouble $ evalNodeRef env block l
        p = 1 / (1 + exp (-l'))
densityPNode _ _ Dist{} _ = error "unrecognised distribution"

densityPNode env block (Loop shp ldag body _) a = LF.product $ do
    let (as,bs) = unzip shp
        as' = evalNodeRef env block <$> as
        bs' = evalNodeRef env block <$> bs
        xs = toList a
    (i,x) <- zip (range (as',bs')) xs
    let inps = Expr.inputs ldag
        env' = zip inps i ++ env
    return $ densityPNode env' (ldag:block) body (fromRational x)
