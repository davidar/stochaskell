{-# LANGUAGE GADTs, ImpredicativeTypes, FlexibleInstances, ScopedTypeVariables,
             FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
module Data.Program where

import Control.Monad.Guard
import Control.Monad.State
import Data.Array.Abstract
import qualified Data.Bimap as Bimap
import Data.Expression
import Data.Expression.Const
import Data.Expression.Eval
import Data.Maybe
import qualified Data.Number.LogFloat as LF
import qualified Data.Random as Rand
import Data.Random.Distribution (logPdf)
import Data.Random.Distribution.Abstract
import Data.Random.Distribution.Categorical (Categorical)
import qualified Data.Random.Distribution.Categorical as Categorical
import GHC.Exts


------------------------------------------------------------------------------
-- PROGRAMS                                                                 --
------------------------------------------------------------------------------

data PNode = Dist { dName :: String
                  , dArgs :: [NodeRef]
                  , typePNode :: Type
                  }
           | Loop { lShape :: [Interval NodeRef]
                  , lDefs  :: DAG
                  , lBody  :: PNode
                  , typePNode :: Type
                  }
           deriving (Eq)

data PBlock = PBlock { definitions :: Block
                     , actions     :: [PNode]
                     , constraints :: [(NodeRef, ConstVal)]
                     }
            deriving (Eq)
emptyPBlock :: PBlock
emptyPBlock = PBlock emptyBlock [] []

-- lift into Block
liftExprBlock :: MonadState PBlock m => State Block b -> m b
liftExprBlock s = do
    PBlock block rhs given <- get
    let (ret, block') = runState s block
    put $ PBlock block' rhs given
    return ret

data Prog t = Prog { fromProg :: State PBlock t }
instance (Eq t) => Eq (Prog t) where p == q = runProg p == runProg q
runProg :: Prog a -> (a, PBlock)
runProg p = runState (fromProg p) emptyPBlock

type NRT = (NodeRef, Type)
type ProgE t = Prog (Expr t)
fromProgE :: (ExprType t) => ProgE t -> State PBlock NRT
fromProgE p = head <$> fromProgExprs p
runProgE :: (ExprType t) => ProgE t -> (NRT, PBlock)
runProgE p = runState (fromProgE p) emptyPBlock

fromProgExprs :: (ExprTuple t) => Prog t -> State PBlock [NRT]
fromProgExprs p = do
  es <- fromExprTuple <$> fromProg p
  is <- mapM (liftExprBlock . fromDExpr) es
  return $ is `zip` map typeDExpr es

runProgExprs :: (ExprTuple t) => Prog t -> ([NRT], PBlock)
runProgExprs p = runState (fromProgExprs p) emptyPBlock

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

dist :: ExprType t => State Block PNode -> ProgE t
dist s = Prog $ do
    d <- liftExprBlock s
    PBlock block rhs given <- get
    put $ PBlock block (d:rhs) given
    let depth = dagLevel $ head block
        k = length rhs
        name = Volatile depth k
        v = expr . return $ Var name (typePNode d)
    return v

instance Distribution Bernoulli (Expr Double) Prog (Expr Bool) where
    sample (Bernoulli p) = dist $ do
        i <- fromExpr p
        return $ Dist "bernoulli" [i] t
      where (TypeIs t) = typeOf :: TypeOf Bool
    sample (BernoulliLogit l) = dist $ do
        i <- fromExpr l
        return $ Dist "bernoulliLogit" [i] t
      where (TypeIs t) = typeOf :: TypeOf Bool

instance Distribution Geometric (Expr Double) Prog (Expr Integer) where
    sample (Geometric p) = dist $ do
        i <- fromExpr p
        return $ Dist "geometric" [i] IntT

instance (ExprType t) => Distribution Categorical (Expr Double) Prog (Expr t) where
    sample cat = dist $ do
        let (ps,xs) = unzip $ Categorical.toList cat
        qs <- mapM fromExpr ps
        ys <- mapM fromExpr xs
        return $ Dist "categorical" (qs ++ ys) RealT

instance Distribution Normal (Expr Double) Prog (Expr Double) where
    sample (Normal m s) = dist $ do
        i <- fromExpr m
        j <- fromExpr s
        return $ Dist "normal" [i,j] RealT


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
    let ids = [ Dummy (length block) i | i <- [1..length sh] ]
        p = ar ! [expr . return $ Var i IntT | i <- ids]
        ((_,t), PBlock (dag:block') [act] []) = runState (fromProgE p) $
            PBlock (DAG (length block) ids Bimap.empty : block) [] []
        loopType = ArrayT Nothing sh t
        loop = Loop sh dag act loopType
    put $ PBlock block' (loop:dists) given
    let name = Volatile (length block - 1) (length dists)
    return (expr . return $ Var name loopType :: Expr f)


------------------------------------------------------------------------------
-- CONDITIONING                                                             --
------------------------------------------------------------------------------

type instance ConditionOf (Prog ()) = Expr Bool
instance MonadGuard Prog where
    guard cond = Prog $ do -- TODO: weaker assumptions
        (Var (Internal 0 i) _) <- liftExprBlock (fromExpr cond)
        (PBlock (dag:dags) dists given) <- get
        if i /= length (nodes dag) - 1 then undefined else do
          let (Just (Apply "==" [j, Const a] _)) =
                lookup i $ nodes dag
              dag' = dag { bimap = Bimap.deleteR i (bimap dag) }
          put $ PBlock (dag':dags) dists ((j,a):given)


------------------------------------------------------------------------------
-- PROBABILITY DENSITIES                                                    --
------------------------------------------------------------------------------

density :: (ExprTuple t) => Prog t -> t -> LF.LogFloat
density prog vals = flip densityPBlock pb $ do
    (d,e) <- unify rets vals
    let v = evalD [] e
    unifyD [] d v
  where (rets, pb) = runProg prog

densityPBlock :: Env -> PBlock -> LF.LogFloat
densityPBlock env (PBlock block refs _) = LF.product $ do
    (i,d) <- zip [0..] $ reverse refs
    let ident = Volatile (dagLevel $ head block) i
    return $ case lookup ident env of
      Just val -> densityPNode env block d val
      Nothing  -> LF.logFloat 1

densityPNode :: Env -> Block -> PNode -> ConstVal -> LF.LogFloat
densityPNode env block (Dist "normal" [m,s] _) x =
    LF.logToLogFloat $ logPdf (Rand.Normal m' s') (toDouble x)
  where m' = toDouble $ evalNodeRef env block m
        s' = toDouble $ evalNodeRef env block s
densityPNode env block (Dist "bernoulli" [p] _) x =
    LF.logFloat (if toBool x then p' else 1 - p')
  where p' = toDouble $ evalNodeRef env block p
densityPNode env block (Dist "bernoulliLogit" [l] _) a
    | x == 1 = LF.logFloat p
    | x == 0 = LF.logFloat (1 - p)
    | otherwise = LF.logFloat 0
  where x = toRational a
        l' = toDouble $ evalNodeRef env block l
        p = 1 / (1 + exp (-l'))
densityPNode env block (Dist "categorical" cats _) x = LF.logFloat $ toDouble p
  where n = length cats `div` 2
        ps = evalNodeRef env block <$> take n cats
        xs = evalNodeRef env block <$> drop n cats
        p = fromMaybe 0 . lookup x $ zip xs ps
densityPNode env block (Dist "geometric" [t] _) x = p * q^k
  where t' = toDouble $ evalNodeRef env block t
        p = LF.logFloat t'
        q = LF.logFloat (1 - t')
        k = toInteger x
densityPNode _ _ (Dist d _ _) _ = error $ "unrecognised density "++ d

densityPNode env block (Loop shp ldag body _) a = LF.product
    [ densityPNode (zip inps i ++ env) block' body (fromRational x)
    | (i,x) <- evalRange env block shp `zip` toList a ]
  where inps = inputs ldag
        block' = ldag : drop (length block - dagLevel ldag) block


------------------------------------------------------------------------------
-- SAMPLING                                                                 --
------------------------------------------------------------------------------

sampleP :: (ExprTuple t) => Prog t -> IO [ConstVal]
sampleP p = do
    env <- samplePNodes [] block idents
    return $ evalTuple env rets
  where (rets, PBlock block refs _) = runProg p
        idents = [ (Volatile (dagLevel $ head block) i, d)
                 | (i,d) <- zip [0..] $ reverse refs ]

samplePNodes :: Env -> Block -> [(Id, PNode)] -> IO Env
samplePNodes env _ [] = return env
samplePNodes env block ((ident,node):rest) = do
    val <- samplePNode env block node
    let env' = (ident, val) : env
    samplePNodes env' block rest

samplePNode :: Env -> Block -> PNode -> IO ConstVal
samplePNode env block (Dist "normal" [m,s] _) = fromDouble <$> normal m' s'
  where m' = toDouble $ evalNodeRef env block m
        s' = toDouble $ evalNodeRef env block s
samplePNode env block (Dist "bernoulli" [p] _) = fromBool <$> bernoulli p'
  where p' = toDouble $ evalNodeRef env block p
samplePNode env block (Dist "bernoulliLogit" [l] _) = fromBool <$> bernoulli p'
  where l' = toDouble $ evalNodeRef env block l
        p' = 1 / (1 + exp (-l'))
samplePNode env block (Dist "categorical" cats _) = fromRational <$> categorical (zip ps xs)
  where n = length cats `div` 2
        ps = toDouble . evalNodeRef env block <$> take n cats
        xs = toRational . evalNodeRef env block <$> drop n cats
samplePNode env block (Dist "geometric" [p] _) = fromInteger <$> geometric p'
  where p' = toDouble $ evalNodeRef env block p
samplePNode _ _ (Dist d _ _) = error $ "unrecognised distribution "++ d

samplePNode env block (Loop shp ldag hd _) = flatten <$> sequence arr
  where inps = inputs ldag
        block' = ldag : drop (length block - dagLevel ldag) block
        arr = [ samplePNode (zip inps idx ++ env) block' hd | idx <- evalRange env block shp ]

mh :: forall r. ExprTuple r => Prog r -> (r -> Prog r) -> r -> IO r
mh target proposal x = do
  y' <- sampleP $ proposal x
  let y = fromConstVals y' :: r
      f = density target
      q = density . proposal
      a = LF.fromLogFloat $ (f y * q y x) / (f x * q x y)
  accept <- bernoulli $ if a > 1 then 1 else a
  if accept then return y else return x
