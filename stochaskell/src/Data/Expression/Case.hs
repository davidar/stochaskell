{-# LANGUAGE ScopedTypeVariables #-}
module Data.Expression.Case where

import Control.Monad.State
import Data.Expression hiding (const)
import Data.Expression.Extract

caseZ :: forall t. (ExprTuple t) => Z -> [t] -> t
caseZ k alts = toExprTuple . entupleD n $ caseD (erase k) (const . fromExprTuple <$> alts)
  where TupleSize n = tupleSize :: TupleSize t

caseD :: DExpr -> [[DExpr] -> [DExpr]] -> DExpr
caseD e fs = DExpr $ do
  k <- fromDExpr e
  case k of
    Data c args _ -> do
      block <- get
      let f = fs !! c
          args' = reDExpr emptyEEnv block <$> args
      js <- sequence $ fromDExpr <$> f args'
      return $ case js of
        [j] -> j
        _ -> Data 0 js . TupleT $ typeRef <$> js
    _ -> caseD' k fs

caseD' :: NodeRef -> [[DExpr] -> [DExpr]] -> State Block NodeRef
caseD' k fs = do
  d <- getNextLevel
  let tss = case typeRef k of
        UnionT t -> t
        IntT -> repeat []
        _ -> error $ "case "++ show k
  cases <- sequence $ do
    (ts,f) <- zip tss fs
    let ids = [(Dummy d i, t) | (i,t) <- zip [0..] ts]
        args = [DExpr . return $ Var i t | (i,t) <- ids]
    return . runLambda ids . sequence $ fromDExpr <$> f args
  dag <- gets topDAG
  if varies dag [k] || variesLambda' dag `any` cases
  then simplify $ Case k cases (tupleT . foldr1 (zipWith coerce) $ map typeRef . fHead <$> cases)
  else liftBlock $ caseD' k fs

fromCase :: forall c t. (Constructor c, ExprTuple t) => (c -> t) -> Expr c -> t
fromCase f e = toExprTuple . entupleD n $ caseD (erase e)
  [fromExprTuple . f . construct Expression c | c <- cs]
  where Tags cs = tags :: Tags c
        TupleSize n = tupleSize :: TupleSize t

caseOf :: (Constructor c, ExprTuple t) => (Expr c -> t) -> Expr c -> t
caseOf f = fromCase (f . fromConcrete)
